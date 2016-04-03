package code.model

import scala.collection._
import scala.language.implicitConversions
import scala.util.Random
import scala.xml.Node
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import net.liftweb.common.{Box, Loggable}
import net.liftweb.json._
import net.liftweb.util.Helpers.tryo
import net.liftweb.record.MetaRecord
import net.liftweb.record.field._
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props
import org.squeryl.annotations._

import code.model.Product.{fetchByStoreCategory, fetchByStore}
import code.model.Inventory.fetchInventoriesByStore

class Store  private() extends IStore with ErrorReporter with Persistable[Store]
  with LcboJSONExtractor[Store] with CreatedUpdated[Store] with Loggable  {

  @Column(name="pkid")
  override val idField = new LongField(this, 0)  // our own auto-generated id
  val lcbo_id = new LongField(this) // we don't share same PK as LCBO!

  // for Persistable
  override def table(): org.squeryl.Table[Store] = Store.table()
  override def cache() = Store.storesCache
  override def LcboIdsToDBIds() = Store.LcboIdsToDBIds
  override def pKey: Long = idField.get
  override def lcboId: Long = lcbo_id.get
  override def setLcboId(id: Long): Unit = lcbo_id.set(id)
  override def meta = Store

  override def MaxPerPage = Store.MaxPerPage
  override def getCachedItem: (IStore) => Option[IStore] =  s => Store.getItemByLcboId(s.lcboId)

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
  override def isDead = is_dead.get
  override def addressLine1 = address_line_1.get

  //products is a StatefulManyToMany[Product,Inventory], it extends Iterable[Product]
  lazy val storeProducts = MainSchema.inventories.leftStateful(this)
  private def inventories = storeProducts.associations

  // following three caches leverage ORM's stateful cache of storeProducts and inventories above (which are not presented as map but as slower sequence;
  // we organize as map for faster access).
  // They're recomputed when needed by the three helper functions that follow.
  private val productsCache = TrieMap[Long, IProduct]()  // keyed by lcboId
  private val productsCacheByCategory = TrieMap[String, IndexedSeq[IProduct]]()
  private val inventoryByProductId = TrieMap[Long, Inventory]()
  private val getCachedInventoryItem: (Inventory) => Option[Inventory] = { (inv: Inventory) => inventoryByProductId.get(inv.productid) }

  private def productsByLcboId: Map[Long, Product] =
    storeProducts.toIndexedSeq.groupBy(_.lcboId).mapValues(_.head)

  private def productsByCategory: Map[String, IndexedSeq[Product]] =
    storeProducts.toIndexedSeq.groupBy(_.primaryCategory)

  private def getInventories: Map[Long, Inventory] =
    inventories.toIndexedSeq.map { inv => inv.productid -> inv } (breakOut)  // moderately slow because of iteration

  private def addToCaches(items: IndexedSeq[IProduct]) = {
    productsCache ++= items.groupBy(_.lcboId).mapValues(_.head) // update local store specific caches after having updated global cache for all products
    productsCacheByCategory ++= items.groupBy(_.primaryCategory)
  }

  private def emptyInventory =
    inventories.toIndexedSeq.forall(_.quantity == 0)

  /**
    * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
    * Select a random product that matches the parameters subject to a max sample size.
    *
    * @param category a String such as beer, wine, mostly matching primary_category at LCBO, or an asset category (for query only not to compare results and filter!).
    * @param requestSize a number representing how many items we need to sample
    * @return quantity found in inventory for product and the product
    */
  def recommend(category: String, requestSize: Int): Box[Iterable[(IProduct, Long)]] = {
    /**
      *
      * @param lcboProdCategory specifies the expected value of primary_category on feedback from LCBO. It's been known that they would send a Wiser's Whiskey on a wine request.
      * @return 0 quantity found in inventory for product (unknown to be resolved in JS) and the product
      */
    def getSerialResult(lcboProdCategory: String) = {
      val prods = fetchByStoreCategory(lcboId, category, Store.MaxSampleSize) // take a hit of one go to LCBO, querying by category, no more.
      val permutedIndices = Random.shuffle[Int, IndexedSeq](prods.indices)
      val seq = for (id <- permutedIndices;
                     p = prods(id)) yield p
      seq.filter { _.primaryCategory == lcboProdCategory}.
        take(requestSize).
        zip(Seq.fill(requestSize)(0.toLong))  // filter by category before take as LCBO does naive (or generic) pattern matching on all fields
      // and then zip with list of zeroes because we are too slow to obtain inventories.
    }

    // we could get errors going to LCBO, this tryo captures those.
    tryo {
      val lcboProdCategory = LiquorCategory.toPrimaryCategory(category) // transform to the category LCBO uses on product names in results (more or less upper case such as Beer)
      val matchingKeys = productsCacheByCategory.getOrElse(lcboProdCategory, IndexedSeq[Product]()).map(_.lcboId)
      val inStockItems =
      for (id <- matchingKeys;
           p <- productsCache.get(id);
           inv <- inventoryByProductId.get(p.pKey);
           q = inv.quantity if q > 0) yield (p, q)

      // products are loaded before inventories and we might have none
      asyncLoadCache() // if we never loaded the cache, do it (fast lock free test). Note: useful even if we have product of matching inventory
      val cached = Random.shuffle(inStockItems).take(requestSize)
      if (cached.nonEmpty) cached
      else getSerialResult(lcboProdCategory)
    }
  }

  // generally has side effect to update database with more up to date content from LCBO's (if different)
  private def loadCache(): Unit = {
    def fetchProducts() = addToCaches( fetchByStore(lcboId) )

    def fetchInventories() = {
      val box = tryo {
        fetchInventoriesByStore(
          uri = s"$LcboDomainURL/inventories",
          LcboStoreId = lcboId,
          getCachedInventoryItem,
          inventoryByProductId.toMap)
      }
      if (checkUnitErrors(box, fullContextErr("inventories") )) refreshInventories()
    }
    val fetches = Future {
      fetchProducts() // fetch and then make sure model/Squeryl classes update to DB and their cache synchronously, so we can use their caches.
    } andThen {
      case x => fetchInventories() // similarly for inventories and serialize intentionally because of Ref.  if no exception was thrown
    }
    fetches foreach {
      case m =>
        //We've persisted along the way for each LCBO page ( no need to refresh because we do it each time we go to DB)
        logger.debug(s"loadCache async work succeeded for $lcboId")
        if (emptyInventory) {
          logger.warn(s"got no product inventory for storeId $lcboId !") // No provision for retrying.
        }
    }
    for ( f <- fetches.failed)  {
      logger.info(s"loadCache explicitly failed for $lcboId cause $f")
    }
    logger.info(s"loadCache async launched for $lcboId") // about 15 seconds, likely depends mostly on network/teleco infrastructure
  }

  private def asyncLoadCache() =
    // A kind of guard: Two piggy-backed requests to loadCache for same store will thus ignore second one.
    if (Store.storeProductsLoaded.putIfAbsent(idField.get, Unit).isEmpty) loadCache()

  def refreshInventories(): Unit = inTransaction {
      storeProducts.refresh // key for whole inventory caching to work!
      inventoryByProductId ++= getInventories
    }

  def refreshProducts(): Unit = inTransaction {
      storeProducts.refresh // key for whole inventory caching and products caching to work!
      productsCache ++= productsByLcboId
      productsCacheByCategory ++= productsByCategory
      inventoryByProductId ++= getInventories
    }
}

object Store extends Store with MetaRecord[Store] {
  private val MaxSampleSize = Props.getInt("store.maxSampleSize", 0)

  private val storesCache: concurrent.Map[Long, Store] = TrieMap()  // primary cache
  override val LcboIdsToDBIds: concurrent.Map[Long, Long] = TrieMap() //secondary dependent cache
  override def table(): org.squeryl.Table[Store] = MainSchema.stores

  override def cacheNewItems(items: Iterable[Store]): Unit = {
    super.cacheNewItems(items)
    storesCache.foreach { case (id, s)  => s.refreshProducts() }  // ensure inventories are refreshed INCLUDING on start up.
  }

  private val storeProductsLoaded: concurrent.Map[Long, Unit] = TrieMap()
  // effectively a thread-safe lock-free set, which helps avoiding making repeated requests for cache warm up for a store.

  override def getCachedItem: (IStore) => Option[IStore] =  s => getItemByLcboId(s.lcboId)
  def availableStores: Set[Long] = storesCache.toMap.keySet
  def lcboIdToDBId(l: Int): Option[Long] = LcboIdsToDBIds.get(l)
  def storeIdToLcboId(s: Long): Option[Long] = storesCache.get(s).map(_.lcboId)
  def getStore(s: Long): Option[IStore] = storesCache.get(s)
  def getItemByLcboId(id: Long): Option[IStore] =
    for (dbId <- LcboIdsToDBIds.get(id);
         s <- storesCache.get(dbId)) yield s

  override def MaxPerPage = Props.getInt("store.lcboMaxPerPage", 0)

    /* Convert a store to XML */
  implicit def toXml(st: Store): Node =
    <store>{Xml.toXml(st.asJValue)}</store>

  private def getStores(dbStores: Map[Long, Store]): Unit = {
    def fullContextErr( m: String,  ex: String): String =
      s"Problem loading LCBO stores into cache with message '$m' and exception error '$ex'"
    def briefContextErr(): String =
      "Problem loading LCBO stores into cache, none found"
    val box = tryo {
      val items =  collectItemsOnPages(s"$LcboDomainURL/stores", Seq("where_not" -> "is_dead"))
      synchDirtyAndNewItems(items, getCachedItem, dirtyPredicate)
      logger.debug(s"done loading stores from LCBO")
      items // nice to know if it's empty, so we can log an error in that case. That's captured by box and looked at within checkErrors using briefContextErr.
    }
    checkErrors(box, fullContextErr, briefContextErr)
  }

  def init(): Unit = {
    logger.info("Store.init start")
    load()
    logger.info("Store.init end")
  }

  override def load(): Unit = inTransaction {
    val items = from(table())(s => select(s))
    cacheNewItems(items)
    asyncGetStores(items.map { s => s.pKey -> s }(breakOut)) // the initial db init is long and synchronous, long because of loading Many-to-Many stateful state, depending on storage data
  }

  private def asyncGetStores(x: Map[Long, Store]) = {
    val fut = Future { getStores(x) }
    fut foreach {
      case m =>
        logger.info(s"asyncGetStores (asynch for Store.init) completed")
    }
    fut.failed foreach {
      case f =>
        logger.error(s"asyncGetStores explicitly failed with cause $f") // pretty fatal at this point.
    }
  }

 def findAll(): Iterable[Store] = storesCache.values

}