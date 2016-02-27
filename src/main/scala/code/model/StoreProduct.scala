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
  private val DBSelectPageSize = Props.getInt("inventory.DBRead.BatchSize", 1000)
  private implicit val formats = net.liftweb.json.DefaultFormats

  // only update after confirmed to be in database!
  private val allStoreProductsFromDB: concurrent.Map[Int, Map[Int, StoreProduct]] = TrieMap()

  def getInventories(storeId: Int): Map[Int, StoreProduct] =
    allStoreProductsFromDB.get(storeId).getOrElse(Map())


  def cachedStoreProductIds: Set[(Int, Int)] = {
    { for ( (s, v) <- allStoreProductsFromDB;
         p <- v.keysIterator) yield (s, p) }.toSet
  }

  def getProductIdsByStore: Map[Int, IndexedSeq[Int]] =
    allStoreProductsFromDB.map { case (storeId, m) => storeId -> m.keys.toIndexedSeq }

  def storeIdsWithCachedProducts: Set[Int] = allStoreProductsFromDB.filter{ case(k,m) => m.nonEmpty}.keySet

  def hasCachedProducts(storeId: Int) = storeIdsWithCachedProducts contains storeId

  def init(maxStoreId: Int): Unit =  { // done on single thread on start up.
    def loadBatch(offset: Int, pageSize: Int): IndexedSeq[StoreProduct] = {
      from(storeProducts)(sp =>
        select(sp)
        orderBy(sp.storeid)).
        page(offset, pageSize).toVector
    }
    inTransaction {
      val total = {from(storeProducts)( _ =>
        compute(count))}.toInt

      logger.debug(s"storeProducts row count: $total")

      for (i <- 0 to (total / DBSelectPageSize);
           inventories = loadBatch(i * DBSelectPageSize, DBSelectPageSize)) {
        val invsByStore: Map[Int, IndexedSeq[StoreProduct]] = inventories.groupBy(_.storeid.get)
        invsByStore.foreach{ case (s, prods) =>
          val newMap = prods.map{ sp => sp.productid.get -> sp}.toMap[Int, StoreProduct]
          allStoreProductsFromDB.get(s).fold {
            allStoreProductsFromDB.putIfAbsent(s, newMap)
          } { m => allStoreProductsFromDB.put(s,  m ++ newMap)
          }
        }
      }
      logger.debug(s"storeProducts loaded over : ${total/DBSelectPageSize} pages")

      // GC and JVM wrestle match end (not enough, sigh)
    }
  }

  def update(storeId: Int, theStores: Iterable[StoreProduct]) = {
    val idPairs = cachedStoreProductIds
    val (existingInventories, newInventories) = theStores.partition{ sp: StoreProduct => idPairs.contains(storeId, sp.productid.get) }
    updateStoreProducts(existingInventories)
    insertStoreProducts(newInventories)
  }

  def emptyInventoryForStore(storeId: Int): Boolean = {
    allStoreProductsFromDB.get(storeId).fold{true}{ m => !m.values.exists(_.quantity.get > 0 )}
  }

  def getStoreProductQuantity(storeId: Int, prodId: Int): Option[Int] = {
    val it: Iterable[Option[StoreProduct]] = allStoreProductsFromDB.get(storeId).map{ _.get(prodId) }
    it.headOption.getOrElse(None).map(_.quantity.get)
  }

  def create(inv: InventoryAsLCBOJson): StoreProduct =
    createRecord.
      storeid(inv.store_id).
      productid(inv.product_id).
      quantity(inv.quantity)

  // @see http://squeryl.org/occ.html
  def updateStoreProducts(myStoreProducts: Iterable[StoreProduct]) = {
    myStoreProducts.toVector.grouped(DBBatchSize).
      foreach { x =>
        inTransaction { storeProducts.forceUpdate(x) }
        // @see http://squeryl.org/occ.html (two threads might fight for same update and if one is stale, that could be trouble with the regular update

        val invsByStore: Map[Int, IndexedSeq[StoreProduct]] = x.groupBy(_.storeid.get)
        for (s <- invsByStore.keys;
             receivedInvs = invsByStore(s);
             inventoriesByProd = receivedInvs.groupBy {_.productid.get } map { case (k,v) => (k, v.head)}  // groupBy will give us a head.
        ) {
          allStoreProductsFromDB.get(s).fold{
            allStoreProductsFromDB.putIfAbsent(s, inventoriesByProd)
          } { oldInvs =>
            val (clean, dirty) = oldInvs.partition( {p: ((Int, StoreProduct)) => !inventoriesByProd.keySet.contains(p._1) })
            val replaced: Map[Int, StoreProduct] = dirty.map{case (k,v) => (k, inventoriesByProd(k))}
            val completelyNew = inventoriesByProd.filterKeys{k: Int => !oldInvs.keySet.contains(k)}
            allStoreProductsFromDB.put(s,  clean ++ replaced ++ completelyNew)

          }
        }
      }
  }

  def insertStoreProducts( myStoreProducts: Iterable[StoreProduct]): Unit = {
    // Do special handling to filter out duplicate keys, which would throw.
    def insertBatch(filtered: Iterable[StoreProduct]): Unit =  {
      try { // the DB could fail for PK or whatever other reason.
        inTransaction { storeProducts.insert(filtered) }
        // update in memory for next caller who should be blocked, also in chunks to be conservative.
        val indexedFiltered = filtered.toVector.groupBy(_.storeid.get)
        indexedFiltered.foreach{ case (storeId, values) =>
          val prodsWithInventory: Map[Int, StoreProduct] = values.map{sp: StoreProduct => sp.productid.get -> sp}(collection.breakOut): Map[Int, StoreProduct]
          if (allStoreProductsFromDB.isDefinedAt(storeId)) {
            allStoreProductsFromDB.put(storeId, allStoreProductsFromDB(storeId) ++ prodsWithInventory)
          }
          else {  // first time we add products for a store in memory
            allStoreProductsFromDB.putIfAbsent(storeId, prodsWithInventory)
          }
        }
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

    // synchronize on object StoreProduct as clients are from different threads
    // first evaluate against cache (assumed in synch with DB) what's genuinely new, by satisfying ref. integrity constraints.
    // Then, annoying filter to ensure uniqueness (see Product, it's easier), preventing duplicate key violation
    def referentialIntegrity (sp: StoreProduct): Boolean = {
      val s = sp.storeid.get
      val p = sp.productid.get
      Product.cachedProductIds.contains(p) && (!allStoreProductsFromDB.contains(s) || !allStoreProductsFromDB(s).contains(p))
      // we don't require store to be in hash because we'll enter it when introducing products for first time
      // That hash is for inventories not for stores. It's assumed all stores are in memory on start up.
      // That's why there's asymetry with Product where we require it to be there.
    }
    val RIEntries = myStoreProducts.filter { referentialIntegrity }.toVector
    val filteredByStoreAndProduct = RIEntries.groupBy{ sp: StoreProduct => (sp.storeid.get, sp.productid.get): (Int, Int)}
    val filtered: Iterable[StoreProduct] = filteredByStoreAndProduct.map { case (k,v) => v.head } // remove duplicate( using head, which exists because of groupBy)
    // break it down and then serialize the work.
    filtered.grouped(DBBatchSize).foreach { insertBatch }
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
