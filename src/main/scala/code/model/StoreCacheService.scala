package code.model

import cats.implicits._
import code.model.Product.fetchByStore
import code.model.Inventory.loadInventoriesByStore
import code.model.GlobalLCBO_IDs._
import code.model.Store.CategoryShowKeyPairVals
import code.model.utils.RetainSingles._
import net.liftweb.common.Loggable
import net.liftweb.squerylrecord.RecordTypeMode._
import scala.collection.{IndexedSeq, Iterable}
import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future, blocking}
import scala.util.{Failure, Success}
import ShowKeyPair._

/**
  * Created by philippederome on 2016-07-31.
  * Provides a cache service for the Store class. Keeps track of products by themselves or by cateory,
  * and inventory (count) in a store. It uses TrieMap to do so.
  */
trait StoreCacheService extends Loggable {
  /**
    * StringFormatter formats an input String within a larger fixed message whose format expects a single String input.
    */
  type StringFormatter = String => String

  /**
    * cache of products, keyed by the LCBO product id.
    */
  val productsCache: TrieMap[LCBO_KEY, IProduct]

  /**
    * cache of inventories keyed by our own primary key of the product.
    */
  val inventoryByProductId: TrieMap[P_KEY, Inventory]

  /**
    * cache of product ids by category, a category index
    */
  val categoryIndex: TrieMap[String, IndexedSeq[ShowKeyPairVals[P_KEY]]]

  private val getCachedInventoryItem: Inventory => Option[Inventory] =
    inv => inventoryByProductId.get(inv.productid.PKeyID)

  /**
    *
    * @return store identifier as published by LCBO
    */
  def lcboKey: LCBO_KEY

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
    * @return nothing (Unit), side effect is to update caches. This is done asynchronously.
    */
  def loadCache()(implicit ec: ExecutionContext): Unit = {
    val productsContext: StringFormatter = s => s"Problem loading products into cache with exception error $s"
    val inventoriesContext: StringFormatter = s => s"Problem loading inventories into cache for '$lcboKey' with exception error $s"

    // serialize products then inventories intentionally because of Ref.Integrity (inventory depends on valid product)
    // @see http://docs.scala-lang.org/overviews/core/futures.html (Blocking inside a Future) or Nedelcu Scala's Best practices
    val loads = for {
      _ <- Future (blocking (loadProducts (productsContext)))
      _ <- Future (blocking (loadInventories (inventoriesContext)))
    } yield ()
    logger.info(s"loadCache async launched for $lcboKey") // about 15 seconds, likely depends mostly on network/teleco infrastructure
    loads onComplete {
      case Success(()) => // We've persisted along the way for each LCBO page ( no need to refresh because we do it each time we go to DB)
        logger.debug(s"loadCache async work succeeded for $lcboKey")
        if (emptyInventory) logger.warn(s"got no product inventory for storeId $lcboKey !") // No provision for retrying.
      case Failure(f) => logger.info(s"loadCache explicitly failed for $lcboKey cause ${f.getMessage}")
    }
  }

  /**
    * Adds in memory items for performance; intended usage is after a database load on initialization.
    * @param items an indexed sequence of products we obtain that we can cache in memory.
    * @return nothing (Unit), side effect to internal caches
    */
  def addToCaches(items: IndexedSeq[IProduct]): Unit = {
    productsCache ++= asMap(items, {p: IProduct => p.lcboKey})
    // project the products to category+key pairs, group by category yielding sequences of category, keys and retain only the key pairs in those sequences.
    // The map construction above technically filters outs from items if there are duplicate keys, so reuse same collection below (productsCache.values)
    categoryIndex ++= productsCache.values.
      map(prod => CategoryShowKeyPairVals(prod.primaryCategory, prod.showKeyPairVals)).toIndexedSeq.
      groupBy(_.category).
      mapValues(_.map(_.keys))
  }

  private def loadProducts(formatter: StringFormatter): Unit = {
    val theProducts = fetchByStore(lcboKey)
    val logErrorOnFail = { t: Throwable => errHandler(t, formatter) }
    val cacheOnSuccess = { items: IndexedSeq[IProduct] =>
        if (items.isEmpty) logger.error("Problem loading products into cache, none found")
        addToCaches(items)
      }

    theProducts.fold[Unit]( logErrorOnFail, cacheOnSuccess)
  }

  private def loadInventories(formatter: StringFormatter): Unit = {

    def cacheInventoriesWithORM(inventories: UpdatedAndNewInventories): Either[Throwable, Unit] =
      Either.catchNonFatal(
        inTransaction {
          // bulk update the ones needing an update and then bulk insert the ones
          // MainSchema actions of update and insert provide an update to database combined to ORM caching, transparent to us
          (ORMExecutor.execute[Inventory, Iterable](MainSchema.inventories.update, inventories.updatedInvs)
            |+| ORMExecutor.execute[Inventory, Iterable](MainSchema.inventories.insert, inventories.newInvs)).
            fold({err: String => throw new Throwable(err)}, (Unit) => ())
        }
      )

    // load @ LCBO, store to DB and cache into ORM stateful caches, trap/log errors, and if all good, refresh our own store's cache.
    // we chain errors using flatMap (FP style).
    val loadAndCache = loadInventoriesByStore(
      webApiRoute = "/inventories",
      get = getCachedInventoryItem,
      mapByProductId = inventoryByProductId.toMap,
      params = Seq("store_id" -> lcboKey, "where_not" -> "is_dead")).
      flatMap(cacheInventoriesWithORM)

    val failure = { t: Throwable => errHandler(t, formatter) }
    val success = { u: Unit => refreshInventories() }
    loadAndCache.fold[Unit](failure, success)
  }

  private def errHandler(t: Throwable, formatter: StringFormatter) =
    logger.error(formatter(t.toString()))

  private def emptyInventory: Boolean = inventories.toIndexedSeq.forall(_.quantity == 0)
}
