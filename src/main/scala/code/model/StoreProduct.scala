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

  private var storeProductsCache: Vector[Map[Int, StoreProduct]] = Vector()

  // only update after confirmed to be in database!
  private val allStoreProductsFromDB: concurrent.Map[Int, IndexedSeq[ProductWithInventory]] = TrieMap()

  def getInventories: Map[(Int, Int), StoreProduct] = {
    val x = {for ((x,storeIdx) <- storeProductsCache.view.zipWithIndex;
         productid <- x.keys;
         sp <- x.get(productid)
    ) yield ((storeIdx, productid) -> sp) }
    x.toMap
  }

  def cachedStoreProductIds: Set[(Int, Int)] = {
    {  for ((x, storeIdx) <- storeProductsCache.view.zipWithIndex;
           productid <- x.keys) yield (storeIdx, productid)
    }.toSet
  }

  def getProductIdsByStore: Map[Int, IndexedSeq[Int]] =
    allStoreProductsFromDB.map { case (storeId, sps) => (storeId, sps.map(_.productId)) }

  def storeIdsWithCachedProducts = allStoreProductsFromDB.keys.toSet

  def hasCachedProducts(storeId: Int) = storeIdsWithCachedProducts contains storeId

  def init(maxStoreId: Int): Unit =  { // done on single thread on start up.
    def loadBatch(offset: Int, pageSize: Int): IndexedSeq[StoreProduct] = {
      from(storeProducts)(sp =>
        select(sp)
        orderBy(sp.storeid)).
        page(offset, pageSize).toVector
    }
    storeProductsCache = Range(1, maxStoreId).map{i => Map[Int, StoreProduct]() }.toVector
    inTransaction {
      // GC and JVM wrestle match start
      val total =
      {from(storeProducts)( _ =>
        compute(count))}.toInt
      logger.trace(s"storeProducts row count: $total")

      for (i <- 0 to (total / DBSelectPageSize);
           inventories = loadBatch(i * DBSelectPageSize, DBSelectPageSize)) {
        val invsByStore: Map[Int, IndexedSeq[StoreProduct]] = inventories.groupBy(_.storeid.get)
        for (s <- invsByStore.keys;
          randomInvs = invsByStore(s);
          inventoriesByProd = randomInvs.groupBy {_.productid.get } map { case (k,v) => (k, v.head)}
        ) {
          storeProductsCache = storeProductsCache.updated (s, inventoriesByProd)
        }

        inventories.foreach {sp =>
          if (allStoreProductsFromDB.isDefinedAt(sp.storeid.get)) {
            val oldSeq: IndexedSeq[ProductWithInventory] = allStoreProductsFromDB(sp.storeid.get)
            val idx = oldSeq.indexWhere( _.productId == sp.productid.get)
            if (idx >= 0)
              allStoreProductsFromDB.put(sp.storeid.get,  oldSeq.updated( idx, ProductWithInventory(sp.productid.get, sp.quantity.get)))
          }
        }
      }
      logger.trace(s"storeProducts loaded over : ${total/DBSelectPageSize} pages")

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
    storeProductsCache(storeId).exists{ p: (Int, StoreProduct) => p._2.quantity.get > 0}
  }

  def getStoreProductQuantity(storeId: Int, prodId: Int): Option[Int] = {
    if (storeProductsCache.isDefinedAt(storeId))
      storeProductsCache(storeId).get(prodId).map{_.quantity.get}
    else None
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
             randomInvs = invsByStore(s);
             inventoriesByProd = randomInvs.groupBy {_.productid.get } map { case (k,v) => (k, v.head)}
        ) {
          storeProductsCache = storeProductsCache.updated (s, inventoriesByProd)
        }

        x.foreach {sp =>
          if (allStoreProductsFromDB.isDefinedAt(sp.storeid.get)) {
            val oldSeq: IndexedSeq[ProductWithInventory] = allStoreProductsFromDB(sp.storeid.get)
            val idx = oldSeq.indexWhere( _.productId == sp.productid.get)
            if (idx >= 0)
              allStoreProductsFromDB.put(sp.storeid.get,  oldSeq.updated( idx, ProductWithInventory(sp.productid.get, sp.quantity.get)))
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
          val oldOnes = storeProductsCache(storeId)
          val newOnes = values.toIndexedSeq.groupBy{sp => sp.productid.get}.mapValues( _.head)
          storeProductsCache = storeProductsCache.updated(storeId, oldOnes ++ newOnes)
          val prodsWithInventory = values.map(sp => ProductWithInventory(sp.productid.get, sp.quantity.get))
          if (allStoreProductsFromDB.isDefinedAt(storeId)) {
            allStoreProductsFromDB.put(storeId, allStoreProductsFromDB(storeId) ++ prodsWithInventory)
          }
          else {
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
    val entries = cachedStoreProductIds
    val referentialIntegrity: (StoreProduct) => Boolean = { sp =>
      !entries.contains((sp.storeid.get, sp.productid.get)) &&
        Product.cachedProductIds.contains(sp.productid.get)
    }
    val fewerEntries = myStoreProducts.filter { referentialIntegrity }.toVector
    val filteredByStoreAndProduct = fewerEntries.groupBy{ sp: StoreProduct => (sp.storeid.get, sp.productid.get): (Int, Int)}
    val filtered: Iterable[StoreProduct] = filteredByStoreAndProduct.map { case (k,v) => v.head } // remove duplicate( using head)
    // break it down and then serialize the work.
    filtered.grouped(DBBatchSize).foreach { insertBatch }
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
