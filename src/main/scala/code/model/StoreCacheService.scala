package code.model

import cats.data.Xor
import code.model.Product.fetchByStore
import code.model.Inventory.fetchInventoriesByStore
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.Store.CategoryKeyKeeperVals
import code.model.utils.RetainSingles._
import net.liftweb.common.Loggable
import net.liftweb.squerylrecord.RecordTypeMode._

import scala.collection.{IndexedSeq, Iterable}
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by philippederome on 2016-07-31.
  * Provides a cache service for the Store class. Keeps track of products by themselves or by cateory,
  * and inventory (count) in a store. It uses TrieMap to do so.
  */
trait StoreCacheService extends ORMExecutor with Loggable {
  /**
    * StringFormatter formats an input String within a larger fixed message whose format expects a single String input.
    */
  type StringFormatter = String => String

  /**
    * cache of products, keyed by the LCBO product id.
    */
  val productsCache: TrieMap[LCBO_ID, IProduct]

  /**
    * cache of inventories keyed by our own primary key of the product.
    */
  val inventoryByProductId: TrieMap[P_KEY, Inventory]

  /**
    * cache of product ids by category, a category index
    */
  val categoryIndex: TrieMap[String, IndexedSeq[KeyKeeperVals]]

  private val getCachedInventoryItem: Inventory => Option[Inventory] =
    inv => inventoryByProductId.get(P_KEY(inv.productid))

  /**
    *
    * @return store identifier as published by LCBO
    */
  def lcboId: LCBO_ID

  /**
    *
    * @return Unit: side effect to use a mechanism to load inventories from database (must be implemented)
    */
  def refreshInventories(): Unit

  /**
    *
    * @return inventories associated with this store
    */
  def inventories: Iterable[Inventory]

  /**
    * Assuming data may be stale, loads products and inventories, indexing products by category by querying the LCBO using its REST API.
    * Generally has side effect to update database with more up to date content from LCBO's (if different)
    * @return nothing (Unit), side effect is to update caches
    */
  def loadCache(): Unit = {
    val productsContext: StringFormatter = s => s"Problem loading products into cache with exception error $s"
    val inventoriesContext: StringFormatter = s => s"Problem loading inventories into cache for '$lcboId' with exception error $s"
    val loadProdsAndInvs: (StringFormatter, StringFormatter) => Unit = (pFormat, iFormat) => {
      loadProducts(pFormat)
      loadInventories(iFormat)
      // serialize products then inventories intentionally because of Ref.Integrity (inventory depends on valid product)
    }
    val fetches = Future(loadProdsAndInvs(productsContext, inventoriesContext))
    logger.info(s"loadCache async launched for $lcboId") // about 15 seconds, likely depends mostly on network/teleco infrastructure
    fetches onComplete {
      case Success(_) => // We've persisted along the way for each LCBO page ( no need to refresh because we do it each time we go to DB)
        logger.debug(s"loadCache async work succeeded for $lcboId")
        if (emptyInventory) logger.warn(s"got no product inventory for storeId $lcboId !") // No provision for retrying.
      case Failure(f) => logger.info(s"loadCache explicitly failed for $lcboId cause ${f.getMessage}")
    }
  }

  /**
    * Adds in memory items for performance; intended usage is after a database load on initialization.
    * @param items an indexed sequence of products we obtain that we can cache in memory.
    * @return nothing (Unit), side effect to internal caches
    */
  def addToCaches(items: IndexedSeq[IProduct]): Unit = {
    productsCache ++= asMap(items, {p: IProduct => p.lcboId})
    // project the products to category+key pairs, group by category yielding sequences of category, keys and retain only the key pairs in those sequences.
    // The map construction above technically filters outs from items if there are duplicate keys, so reuse same collection below (productsCache.values)
    categoryIndex ++= productsCache.values.
      map(x => CategoryKeyKeeperVals(x.primaryCategory, x: KeyKeeperVals)).toIndexedSeq.
      groupBy(_.category).
      mapValues(_.map(x => x.keys))
  }

  private def loadProducts(contextErrFormatter: String => String): Unit = {
    val theProducts = fetchByStore(lcboId)
    val logErrorOnFail = { t: Throwable => errHandler(t, contextErrFormatter) }
    val cacheOnSuccess = { items: IndexedSeq[IProduct] =>
        if (items.isEmpty) logger.error("Problem loading products into cache, none found")
        addToCaches(items)
      }

    theProducts.fold[Unit]( logErrorOnFail, cacheOnSuccess)
  }

  private def loadInventories(contextErrFormatter: String => String): Unit = {
    def inventoryTableUpdater: (Iterable[Inventory]) => Unit = MainSchema.inventories.update
    def inventoryTableInserter: (Iterable[Inventory]) => Unit = MainSchema.inventories.insert
    // fetch @ LCBO, store to DB and cache into ORM stateful caches, trap/log errors, and if all good, refresh our own store's cache.
    // we chain errors using flatMap (FP fans apparently like this).
    def storeInventories(inventories: UpdatedAndNewInventories): Throwable Xor Unit =
      Xor.catchNonFatal(inTransaction {
        // bulk update the ones needing an update, having made the change from LCBO input
        val updatedOrErrors = execute[Inventory](inventories.updatedInvs, inventoryTableUpdater)
        updatedOrErrors.fold[Unit]({err: String => throw new Throwable(err)}, items => ())

        // bulk insert the ones needing an insert having filtered out duped composite keys
        val insertedOrErrors = execute[Inventory](inventories.newInvs, inventoryTableInserter)
        insertedOrErrors.fold[Unit]({err: String => throw new Throwable(err)}, items => ())
      })

    val getAndStoreInventories = fetchInventoriesByStore(
      webApiRoute = "/inventories",
      getCachedInventoryItem,
      inventoryByProductId.toMap,
      Seq("store_id" -> lcboId, "where_not" -> "is_dead")).flatMap(storeInventories)

    val failure = { t: Throwable => errHandler(t, contextErrFormatter) }
    val success = { u: Unit => refreshInventories() }
    getAndStoreInventories.fold[Unit](failure, success)
  }

  private def errHandler(t: Throwable, contextErrFormatter: String => String) =
    logger.error(contextErrFormatter(t.toString()))

  private def emptyInventory: Boolean = inventories.toIndexedSeq.forall(_.quantity == 0)
}
