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
import scala.collection.{Set, concurrent, Map, Iterable}
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
}

case class ProductWithInventory(productId: Int, quantity: Int)

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
  private val DBBatchSize = Props.getInt("inventory.DBWrite.BatchSize", 100)
  private implicit val formats = net.liftweb.json.DefaultFormats

  // only update after confirmed to be in database!
  private val allStoreProductsFromDB: concurrent.Map[Int, Map[Int, StoreProduct]] = TrieMap()

  def getInventories(storeId: Int): Map[Int, StoreProduct] =
    allStoreProductsFromDB.get(storeId).getOrElse(Map())

  def emptyInventoryForStore(storeId: Int): Boolean = {
    // if that store does not exist in cache, it's false.
    allStoreProductsFromDB.get(storeId).exists{ _.values.forall(_.quantity.get == 0 )}
  }

  def getStoreProductQuantity(storeId: Int, prodId: Int): Option[Int] = {
    val it: Iterable[Option[StoreProduct]] = allStoreProductsFromDB.get(storeId).map{ _.get(prodId) }
    it.headOption.getOrElse(None).map(_.quantity.get)
  }

  def existsStoreProduct(storeId: Int, prodId: Int): Boolean = {
    allStoreProductsFromDB.get(storeId).exists{ _.isDefinedAt(prodId) }
  }

  def getProductIdsByStore: Map[Int, IndexedSeq[Int]] =
    allStoreProductsFromDB.map { case (storeId, m) => storeId -> m.keys.toIndexedSeq }

  def hasProductsByStoreCategory(storeId: Int, category: String) = {
    // common English: There is the cached store such that it has a product key such that its product can be fetched
    // such that the product's primaryCategory is the requested category.
    // No attempt to obfuscate here, seriously!
    allStoreProductsFromDB.get(storeId).exists {
      _.keys.exists {
        Product.getProduct(_).exists {
          _.primaryCategory == category
        }
      }
    }
  }

  def getProductIdsByStoreCategory(storeId: Int, category: String): IndexedSeq[Int] = {
    {
      for (storeMap <- allStoreProductsFromDB.get(storeId).toSeq; // toSeq is to make what follows work on sequences rather than options, as first guy dictates interface
           storeProdId <- storeMap.keys;
           storeProd <- Product.getProduct(storeProdId)
           if storeProd.primaryCategory == category) yield storeProd.lcbo_id.get
    }.toIndexedSeq
  }

  def storeIdsWithCachedProducts: Set[Int] =
    allStoreProductsFromDB.filter{ case(k,m) => m.nonEmpty }.keySet

  def hasCachedProducts(storeId: Int): Boolean =
    allStoreProductsFromDB.get(storeId).exists{ _.nonEmpty }

  def init(availableStores: Set[Int]): Unit = { // done on single thread on start up.
    def loadBatch(storeId: Int): Iterable[StoreProduct] =
      from(storeProducts)(sp =>
        where(sp.storeid === storeId)
        select(sp))

    inTransaction {
      for (storeId <- availableStores;
           inventories = loadBatch(storeId))
      {
        val m: Map[Int, StoreProduct] = inventories.groupBy { _.productid.get}.
          map{ case (k,v) => k -> v.last } // groupBy gives us all the required prod keys and once we have a key, we retain only one StoreProduct to avoid duplicates
        allStoreProductsFromDB.putIfAbsent(storeId, m)
      }
      logger.debug(s"storeProducts loaded over for ${availableStores.size} stores")
    }
  }

  def update(storeId: Int, theStores: Seq[StoreProduct]) = {
    val (existingInventories, newInventories) = theStores.partition { sp => existsStoreProduct(storeId, sp.productid.get) }
    updateStoreProducts(storeId, existingInventories)
    insertStoreProducts(storeId, newInventories)
  }

  def create(inv: InventoryAsLCBOJson): StoreProduct =
    createRecord.
      storeid(inv.store_id).
      productid(inv.product_id).
      quantity(inv.quantity)

  // @see http://squeryl.org/occ.html
  private def updateStoreProducts(storeId: Int, myStoreProducts: Seq[StoreProduct]): Unit = {
    myStoreProducts.grouped(DBBatchSize).
      foreach { x =>
        inTransaction { storeProducts.forceUpdate(x) }
        // @see http://squeryl.org/occ.html (two threads might fight for same update and if one is stale, that could be trouble with the regular update

        val inventoriesByProdId = x.toVector.groupBy {_.productid.get } map { case (k,v) => (k, v.last)}  // groupBy will give us a last.
        val oldVal = allStoreProductsFromDB.putIfAbsent(storeId, inventoriesByProdId)
        oldVal.map { oldInventories => // if we did put when absent, we're not coming here! Lock-free rules!
          val (dirty, clean) = oldInventories.partition( {p => inventoriesByProdId.keySet.contains(p._1) })
          val replaced: Map[Int, StoreProduct] = dirty.map{case (k,v) => (k, inventoriesByProdId(k))}
          val completelyNew = inventoriesByProdId.filterKeys{ id => !oldInventories.keySet.contains(id) }
          allStoreProductsFromDB.put(storeId,  clean ++ replaced ++ completelyNew)
        }
      }
  }

  // this does not check for database, so it's assumed caller is from this class and will have the "sense" to call it only once for a given object.
  private def insertStoreProducts(storeId: Int, myStoreProducts: Seq[StoreProduct]): Unit = {
    // Do special handling to filter out duplicate keys, which would throw.
    def insertBatch(filtered: Iterable[StoreProduct]): Unit = {
      try {
        // the DB could fail for PK or whatever other reason.
        inTransaction { storeProducts.insert(filtered) }
        // update in memory for next caller who should be blocked, also in chunks to be conservative.
        val prodsWithInventory: Map[Int, StoreProduct] = filtered.map { sp => sp.productid.get -> sp }(collection.breakOut): Map[Int, StoreProduct]
        val oldVal = allStoreProductsFromDB.putIfAbsent(storeId, prodsWithInventory)
        oldVal.map { oldInventories => // if we did put when absent, we're not coming here! Lock-free rules!
          allStoreProductsFromDB.put(storeId, oldInventories ++ prodsWithInventory)
        }
      } catch {
        case se: SQLException =>
          val prodIds = filtered.map{sp => sp.productid.get}
          logger.error(s"SQLException $prodIds")
          logger.error("Code: " + se.getErrorCode())
          logger.error("SqlState: " + se.getSQLState())
          logger.error("Error Message: " + se.getMessage())
          logger.error("NextException:" + se.getNextException())
        case e: Exception =>
          logger.error("General exception caught: " + e)
      }
    }
    if (!Store.availableStores.contains(storeId)) {
      logger.error(s"Invalid parameter storeId $storeId not even in memory!")
      return
    } // store does not even exist! Forget about it. Apology for being imperative with "return" ...

    val productIds = Product.cachedProductIds // evaluate once
    // synchronize on object StoreProduct as clients are from different threads
    // first evaluate against cache (assumed in synch with DB) what's genuinely new, by satisfying ref. integrity constraints.
    // Then, annoying filter to ensure uniqueness (see Product, it's easier), preventing duplicate key violation
    def referentialIntegrity(sp: StoreProduct): Boolean = {
      val p = sp.productid.get
      productIds.contains(p) && !allStoreProductsFromDB(storeId).contains(p)
    }

    // filter on expected store, sequence it for efficient groupBy, then group by product to filter out duplicates as we'll select only
    // last one for each product as we don't want more than one (RI problem on duped insert for FK productid if we take it)
    val filteredForUnique: Set[StoreProduct] = myStoreProducts.filter{ _.storeid.get == storeId}.
      toVector.groupBy { _.productid.get }.
      map { case (k,v) => v.last }.toSet // remove duplicate( using last, i.e. most up to date, which exists because of groupBy)
    // Since we know we have this store in inventory do a check on both sp combo and product
    // but if we have a new store not in inventory but that exists, make sure the product exists.
    // We already filtered for input myStoreProducts only containing expected storeId.
    val filteredForRI: Set[StoreProduct] = {
      if (allStoreProductsFromDB.contains(storeId))
        filteredForUnique.filter { referentialIntegrity }
      else filteredForUnique.filter {sp => productIds.contains(sp.productid.get) }
    }
    // break it down in chunks
    filteredForRI.grouped(DBBatchSize).foreach { insertBatch }
  }

  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  def inventoryForStoreProduct(storeId: Int, productId: Int): InventoryAsLCBOJson = {
    val uri = s"$LcboDomainURL/stores/$storeId/products/$productId/inventory"
    logger.debug(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    (parse(pageContent) \ "result").extract[InventoryAsLCBOJson] // more throws
  }

}
