package code.model

import java.io.IOException
import java.net.SocketTimeoutException
import java.sql.SQLException

import code.Rest.pagerRestClient
import code.model.MainSchema._
import net.liftweb.common.Loggable
import net.liftweb.json.JsonParser.{ParseException, parse}
import net.liftweb.json.MappingException
import net.liftweb.record.field.{LongField,IntField}
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props
import org.squeryl.annotations.Column

import scala.collection.concurrent.TrieMap
import scala.collection.{Set, concurrent, Map, Iterable, breakOut}
import scala.language.implicitConversions

case class InventoryAsLCBOJson(product_id: Int,
                               store_id: Int,
                               is_dead: Boolean,
                               updated_on: String,
                               quantity: Int) {
  def removeNulls: InventoryAsLCBOJson = {
    // remove LCBO's poisoned null strings
    def notNull(s: String) = if (s eq null) "" else s

    InventoryAsLCBOJson(
      product_id,
      store_id,
      is_dead,
      notNull(updated_on),
      quantity)
  }
  // dbStoreProducts is assumed bound to a StoreId, so we can key simply by product_id
  def getProduct(dbStoreProducts: Map[Int, StoreProduct]) = dbStoreProducts.get(product_id)
}

case class ProductWithInventory(product: Product, storeProduct: StoreProduct)

class StoreProduct private() extends Record[StoreProduct] with KeyedRecord[Long] with CreatedUpdated[StoreProduct] {
  def meta = StoreProduct

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  lazy val product = MainSchema.productToStoreProducts.right(this)

  val storeid = new IntField(this)
  val productid = new IntField(this)
  val quantity = new IntField(this)

  def isDirty(inv: InventoryAsLCBOJson): Boolean =
    quantity.get != inv.quantity

  def copyAttributes(inv: InventoryAsLCBOJson): StoreProduct = {
    quantity.set(inv.quantity)
    this
  }
}

object StoreProduct extends StoreProduct with MetaRecord[StoreProduct] with pagerRestClient with Loggable {
  private val DBBatchSize = Props.getInt("storeProduct.DBBatchSize", 1)
  private implicit val formats = net.liftweb.json.DefaultFormats

  // thread-safe lock free object
  private val storeProductsCache: concurrent.Map[(Int, Int), StoreProduct] = TrieMap() // only update after confirmed to be in database!
  private val allStoreProductsFromDB: concurrent.Map[Int, IndexedSeq[ProductWithInventory]] = TrieMap()

  def cachedStoreProductIds: Set[(Int, Int)] = storeProductsCache.keySet

  def getProductsByStore: Map[Int, IndexedSeq[Product]] =
    allStoreProductsFromDB.map{case (storeId, iter) => (storeId, iter.map(_.product))}

  def storeIdsWithCachedProducts = allStoreProductsFromDB.keys.toSet
  def hasCachedProducts(storeId: Int) = storeIdsWithCachedProducts contains storeId

  def init(): Unit = {
    val tmp: Map[Int, IndexedSeq[ProductWithInventory]] =
     inTransaction {
       // toVector is essential for decent performance
       import scala.language.postfixOps
       val pairs = from(storeProducts, products)((sp, p) =>
         where(sp.productid === p.lcbo_id)
           select(sp, p))
         .toVector
       logger.trace("StoreProduct.init query done")
       pairs.groupBy(_._1.storeid.get).map {
         case (storeId, vecOfPairs) => storeId -> vecOfPairs.map(pair => ProductWithInventory(pair._2, pair._1))
       }
     }
     allStoreProductsFromDB ++= tmp

     for ((store, it) <- allStoreProductsFromDB;
         inv <- it;
         prod <- Option(inv.product);
         sp <- Option(inv.storeProduct);
         lcbo_id <- Option(prod.lcbo_id.get))
     { storeProductsCache += (store, lcbo_id) -> sp }

  }
  def update(storeId: Int, storeMap: Map[Int, StoreProduct]) = {
    val newSPs = storeMap.values.filter{ sp: StoreProduct => !storeProductsCache.keySet.contains(storeId, sp.productid.get) }
    val existingSPs = storeMap.values.filter{ sp: StoreProduct => storeProductsCache.keySet.contains(storeId, sp.productid.get) }
    updateStoreProducts(existingSPs)
    insertStoreProducts(newSPs)
  }

  def getStoreProduct(storeId: Int, prodId: Int): Option[StoreProduct] =
    storeProductsCache get (storeId, prodId)

  // Uses convenient Squeryl native Scala ORM for a simple 2-table join.
  def getStoreProductsFromDB(storeId: Int): Map[Int, ProductWithInventory] =
    inTransaction {
      val prods = from(storeProducts, products)((sp, p) =>
        where(sp.storeid === storeId and sp.productid === p.lcbo_id  )
          select (p, sp) )
      prods.map({ case (p, sp) => p.lcbo_id.get -> ProductWithInventory(p, sp)})(breakOut)
    }

  def create(inv: InventoryAsLCBOJson): StoreProduct =
    createRecord.
      storeid(inv.store_id).
      productid(inv.product_id).
      quantity(inv.quantity)

  def updateStoreProducts(myStoreProducts: Iterable[StoreProduct]) = {
    myStoreProducts.grouped(DBBatchSize).
      foreach { x =>
        inTransaction {
          storeProducts.update(x)
        }
        storeProductsCache ++= x.map { sp => (sp.storeid.get, sp.productid.get) -> sp }.toMap
      }
  }

  def insertStoreProducts( myStoreProducts: Iterable[StoreProduct]): Unit = {
    // Do special handling to filter out duplicate keys, which would throw.
    def insertBatch(myStoreProducts: Iterable[StoreProduct]): Unit = synchronized { // synchronize on object StoreProduct as clients are from different threads
    // first evaluate against cache (assumed in synch with DB) what's genuinely new.
    // Then, annoying filter to ensure uniqueness (see Product, it's easier)
      val entries = cachedStoreProductIds
      val referentialIntegrity: (StoreProduct) => Boolean = { sp =>
        !entries.contains((sp.storeid.get, sp.productid.get)) &&
          Product.cachedProductIds.contains(sp.productid.get)
      }
      val fewerEntries: IndexedSeq[StoreProduct] = myStoreProducts.filter { referentialIntegrity }.toVector
      // help the peculiar type system...
      val filteredByStoreAndProduct: Map[(Int, Int), IndexedSeq[StoreProduct]] = fewerEntries.groupBy{ sp: StoreProduct => (sp.storeid.get, sp.productid.get): (Int, Int)}
      val filtered: Iterable[StoreProduct] = filteredByStoreAndProduct.map {
        case (k,v) => v.head} // remove duplicate(head)
      // insert them
      try { // the DB could fail for PK or whatever other reason.
          storeProducts.insert(filtered)
        // update in memory for next caller who should be blocked
        storeProductsCache ++= filtered.map { sp => (sp.storeid.get, sp.productid.get) -> sp }.toMap
      } catch {
        case se: SQLException =>
          logger.error("SQLException ")
          logger.error("Code: " + se.getErrorCode())
          logger.error("SqlState: " + se.getSQLState())
          logger.error("Error Message: " + se.getMessage())
          logger.error("NextException:" + se.getNextException())
        case e: Exception =>
          logger.error("General exception caught: " + e)
      }

    }
    inTransaction {
      // break it down and then serialize the work.
      myStoreProducts.grouped(DBBatchSize).foreach { insertBatch }
    }
  }


  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  def inventoryForStoreProduct(storeId: Int, productId: Int): InventoryAsLCBOJson = {
    val uri = s"$LcboDomainURL/stores/$storeId/products/$productId/inventory"
    logger.trace(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    (parse(pageContent) \ "result").extract[InventoryAsLCBOJson] // more throws
  }

}
