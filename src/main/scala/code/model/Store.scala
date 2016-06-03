package code.model

import scala.collection._
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}
import scala.xml.Node
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import net.liftweb.common.Box
import net.liftweb.json._
import net.liftweb.record.MetaRecord
import net.liftweb.record.field._
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props
import org.squeryl.annotations._
import code.model.Product.fetchByStore
import code.model.Inventory.fetchInventoriesByStore
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.prodAdvisor.{ProductAdvisorComponentImpl, ProductAdvisorDispatcher}
import code.model.utils.RNG
import code.model.utils.RetainSingles.asMap


class Store private() extends LCBOEntity[Store] with IStore with ProductAdvisorDispatcher with ProductAdvisorComponentImpl {

  @Column(name="pkid")
  override val idField = new LongField(this, 0)  // our own auto-generated id
  val lcbo_id = new LongField(this) // we don't share same PK as LCBO!

  // for Loader and LCBOEntity
  override def table(): org.squeryl.Table[Store] = Store.table()
  override def cache() = Store.cache
  override def LcboIdToPK() = Store.LcboIdToPK
  override def pKey: P_KEY = P_KEY(idField.get)
  override def lcboId: LCBO_ID = LCBO_ID(lcbo_id.get)
  override def meta = Store

  override def getCachedItem: IStore => Option[IStore] = s => Store.getCachedItem(s)

  val is_dead = new BooleanField(this, false)
  val latitude = new DoubleField(this)
  val longitude = new DoubleField(this)
  val name = new StringField(this, 200) {
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val address_line_1 = new StringField(this, 200) {
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val city = new StringField(this, 30) {
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }

  override def Name = name.get
  override def isDead = is_dead.get
  override def addressLine1 = address_line_1.get

  override def canEqual(other: Any) =
    other.isInstanceOf[Store]

  override def equals(other: Any): Boolean =
    other match {
      case that: Store =>
        if (this eq that) true
        else {
          that.canEqual(this) &&
          (Name == that.Name &&
           isDead == that.isDead &&
           addressLine1 == that.addressLine1 )
        }
      case _ => false
    }

  //products is a StatefulManyToMany[Product,Inventory], it extends Iterable[Product]
  lazy val storeProducts = MainSchema.inventories.leftStateful(this)
  private def inventories = storeProducts.associations

  // following three caches leverage ORM's stateful cache of storeProducts and inventories above (which are not presented as map but as slower sequence;
  // we organize as map for faster access).
  // They're recomputed when needed by the three helper functions that follow.
  override def getProduct(x: LCBO_ID): Option[IProduct] = productsCache.get(x)
  private val productsCache = TrieMap[LCBO_ID, IProduct]()
  private val productsCacheByCategory = TrieMap[String, IndexedSeq[KeyKeeperVals]]()  // don't put whole IProduct in here, just useful keys.
  private val inventoryByProductId = TrieMap[P_KEY, Inventory]()
  override val inventoryByProductIdMap: P_KEY => Option[Inventory] = key => inventoryByProductId.toMap.get(key)

  private val getCachedInventoryItem = {
    inv: Inventory =>
    inventoryByProductId.get(P_KEY(inv.productid))
  }

  override def getProductKeysByCategory(lcboCategory: String) =
    productsCacheByCategory.get(lcboCategory).
      fold(IndexedSeq[KeyKeeperVals]()){ identity }

  private def getInventories: Map[P_KEY, Inventory] =
    asMap(inventories, { i: Inventory => P_KEY(i.productid)} )

  case class CategoryKeyKeeperVals(category: String, keys: KeyKeeperVals) {}
  private def addToCaches(items: IndexedSeq[IProduct]): Unit = {
    productsCache ++= asMap(items, {p: IProduct => p.lcboId})
    // project the products to category+key pairs, group by category yielding sequences of category, keys and retain only the key pairs in those sequences.
    // The map construction above technically filters outs from items if there are duplicate keys, so reuse same collection below (productsCache.values)
    productsCacheByCategory ++= productsCache.values.
      map(x => CategoryKeyKeeperVals(x.primaryCategory, x: KeyKeeperVals)).toIndexedSeq.
      groupBy(_.category).
      mapValues(_.map(x => x.keys))
  }

  def refreshInventories(): Unit = inTransaction {
    storeProducts.refresh // key for whole inventory caching to work!
    inventoryByProductId ++= getInventories
  }

  def refreshProducts(): Unit =  {
    refreshInventories()
    addToCaches(storeProducts.toIndexedSeq)
  }

  private def emptyInventory = inventories.toIndexedSeq.forall(_.quantity == 0)

  def advise(rng: RNG, category: String, requestSize: Int, runner: ProductRunner): Box[Iterable[(IProduct, Long)]] =
    advise(rng, this, category, requestSize, runner)

  // generally has side effect to update database with more up to date content from LCBO's (if different)
  private def loadCache(): Unit = {
    def fetchProducts() = {
      val contextErr = { (err: String) =>
        s"Problem loading products into cache with exception error $err"
      }
      val theProducts = fetchByStore(lcboId)
      lazy val err = checkErrors(theProducts, contextErr, error = "Problem loading products into cache" )
      theProducts.toOption.fold[Unit](logger.error(err))( items => addToCaches _)
    }

    def fetchInventories() = {
      def inventoryTableUpdater: (Iterable[Inventory]) => Unit = MainSchema.inventories.update
      def inventoryTableInserter: (Iterable[Inventory]) => Unit = MainSchema.inventories.insert

      val fullContextErr = (err: String) =>
        s"Problem loading inventories into cache for '$lcboId' with exception error $err"
      // fetch @ LCBO, store to DB and cache into ORM stateful caches, trap/log errors, and if all good, refresh our own store's cache.
      // we chain errors using flatMap (FP fans apparently like this).
      val computation =
        fetchInventoriesByStore(
          webApiRoute = "/inventories",
          getCachedInventoryItem,
          inventoryByProductId.toMap,
          Seq("store_id" -> lcboId, "where_not" -> "is_dead")).
      flatMap { inventories =>
        Try(inTransaction {
          execute[Inventory](inventories.updatedInvs, inventoryTableUpdater) // bulk update the ones needing an update, having made the change from LCBO input
          execute[Inventory](inventories.newInvs, inventoryTableInserter) // bulk insert the ones needing an insert having filtered out duped composite keys
        })
      }
      lazy val err = checkUnitErrors(computation, fullContextErr )
      computation.toOption.fold[Unit](logger.error(err))( (Unit) => refreshInventories())
    }

    val fetches =
      for (p <- Future(fetchProducts()); // fetch and then make sure model/Squeryl classes update to DB and their cache synchronously, so we can use their caches.
           i <- Future(fetchInventories())) yield i // similarly for inventories and serialize intentionally because of Ref.Integrity  if no exception was thrown

    fetches onComplete {
      case Success(_) => //We've persisted along the way for each LCBO page ( no need to refresh because we do it each time we go to DB)
        logger.debug(s"loadCache async work succeeded for $lcboId")
        if (emptyInventory) logger.warn(s"got no product inventory for storeId $lcboId !") // No provision for retrying.
      case Failure(f) => logger.info(s"loadCache explicitly failed for $lcboId cause ${f.getMessage}")
    }
    logger.info(s"loadCache async launched for $lcboId") // about 15 seconds, likely depends mostly on network/teleco infrastructure
  }

  override def asyncLoadCache() =
  // A kind of guard: Two piggy-backed requests to loadCache for same store will thus ignore second one.
    if (Store.storeProductsLoaded.putIfAbsent(idField.get, Unit).isEmpty) loadCache()

}

object Store extends Store with MetaRecord[Store] {
  override val cache: concurrent.Map[P_KEY, Store] = TrieMap()  // primary cache
  override val LcboIdToPK: concurrent.Map[LCBO_ID, P_KEY] = TrieMap() //secondary dependent cache, a.k.a. index
  override def table() = MainSchema.stores
  override def cacheItems(items: Iterable[Store]): Unit = {
    super.cacheItems(items)
    cache.values.foreach( _.refreshProducts())  // ensure inventories are refreshed INCLUDING on start up.
  }
  override def getCachedItem: IStore => Option[IStore] = s => getItemByLcboId(s.lcboId)

  def findAll: Iterable[Store] = cache.values

  private val storeProductsLoaded: concurrent.Map[Long, Unit] = TrieMap()
  // effectively a thread-safe lock-free set, which helps avoiding making repeated requests for cache warm up for a store.

  val queryFilterArgs = getSeq("store.query.Filter")(ConfigPairsRepo.defaultInstance) :+ "per_page" -> Props.getInt("store.lcboMaxPerPage", 0)

  def availableStores = cache.keySet
  def lcboIdToPK(lcboId: LCBO_ID): Option[P_KEY] = LcboIdToPK.get(lcboId)
  def storeIdToLcboId(pKey: P_KEY): Option[LCBO_ID] = cache.get(pKey).map(_.lcboId)
  def getStore(pKey: P_KEY) = cache.get(pKey)
  def getItemByLcboId(lcboId: LCBO_ID) =
    for (pKey <- LcboIdToPK.get(lcboId);
         s <- cache.get(pKey)) yield s

    /* Convert a store to XML, @see Scala in Depth implicit view */
  implicit def toXml(s: Store): Node =
    <store>{Xml.toXml(s.asJValue)}</store>

  private def getStores(): Try[IndexedSeq[Store]] = Try {
      collectItemsAsWebClient("/stores", extract, queryFilterArgs) // nice to know if it's empty, so we can log an error in that case. That's captured by box and looked at within checkErrors using briefContextErr.
    }

  /**
    * synchronous because once the webapp accepts requests, this load must have completed so that the store collection is not empty.
    * Well... Trend is to do everything asynch these days...
    */
  override def load(): Unit = inTransaction {
    def contextErr( ex: String): String =
      s"Problem loading LCBO stores into cache with exception error '$ex'"
    val briefContextErr = "Problem loading LCBO stores into cache, none found"

    logger.info("load start")
    val dbStores = from(table())(item => select(item))
    cacheItems(dbStores)
    // the initial db select is long and synchronous, long because of loading Many-to-Many stateful state, depending on stored data
    val refreshed = getStores()  // improves our cache of stores with latest info from LCBO. In real-world, we might have the app run for long and call getStores async once in a while
    lazy val err = checkErrors(refreshed, contextErr, briefContextErr )
    refreshed.toOption.fold[Unit](logger.error(err))( items => synchDirtyAndNewItems(items, getCachedItem, dirtyPredicate))
    logger.info("load end") // trace because it takes a long time.
  }
}