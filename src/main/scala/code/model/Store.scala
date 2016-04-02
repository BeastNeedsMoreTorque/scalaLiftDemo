package code.model

import java.io.IOException
import java.net.SocketTimeoutException
import java.sql.SQLException

import scala.collection._
import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.util.Random
import scala.xml.Node
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import net.liftweb.common.{Box, Empty, Full, Loggable}
import net.liftweb.json._
import net.liftweb.util.Helpers.tryo
import net.liftweb.record.MetaRecord
import net.liftweb.record.field._
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props
import org.squeryl.annotations._
import org.apache.http.TruncatedChunkException

import code.model.Product.{collectProducts, fetchByStore}

class Store  private() extends IStore with LcboItem[Store, IStore] with Persistable[Store] with Loader[Store] with LcboJSONExtractor[Store] with CreatedUpdated[Store] with Loggable  {

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
  override def getItemByLcboId(id: Long): Option[IStore] =
    Store.getItemByLcboId(id)

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

  private def productsByLcboId: Map[Long, Product] = storeProducts.toIndexedSeq.groupBy(_.lcboId).mapValues(_.head)
  private def productsByCategory: Map[String, IndexedSeq[Product]] = storeProducts.toIndexedSeq.groupBy(_.primaryCategory)
  private def getInventories: Map[Long, Inventory] =
    inventories.toIndexedSeq.map { inv => inv.productid -> inv } (breakOut)  // moderately slow because of iteration

  private def addToCaches(items: IndexedSeq[IProduct]): Unit = {
    productsCache ++= items.groupBy(_.lcboId).mapValues(_.head) // update local store specific caches after having updated global cache for all products
    productsCacheByCategory ++= items.groupBy(_.primaryCategory)
  }
  private def emptyInventory: Boolean =
    inventories.toIndexedSeq.forall(_.quantity == 0)

  def isDirty(s: Store): Boolean = {
    is_dead.get != s.is_dead.get ||
      address_line_1.get != s.address_line_1.get
  }

  /**
    * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
    * Select a random product that matches the parameters subject to a max sample size.
    *
    * @param category a String such as beer, wine, mostly matching primary_category at LCBO, or an asset category (for query only not to compare results and filter!).
    * @param requestSize a number representing how many items we need to sample
    * @return quantity found in inventory for product and the product
    */
  def recommend(category: String, requestSize: Int): Box[Iterable[(Long, IProduct)]] = {
    def getRequestFromCache(matchingKeys: IndexedSeq[Long]): IndexedSeq[(Long, IProduct)] = {
      // get some random sampling.
      val permutedKeys = Random.shuffle(matchingKeys).toStream
      // @see http://stackoverflow.com/questions/13596385/how-to-cut-a-for-comprehension-short-break-out-of-it-in-scala (good use of Stream!)
      val x = for (id <- permutedKeys;
           p <- productsCache.get(id);
           inv <- inventoryByProductId.get(p.pKey);
           q = inv.quantity if q > 0
      ) yield (q,p)
      x.take(requestSize).toIndexedSeq // Stream allows ourselves to limit the use of comprehension to a smaller set bound by the take call here.
    }

    /**
      *
      * @param lcboProdCategory specifies the expected value of primary_category on feedback from LCBO. It's been known that they would send a Wiser's Whiskey on a wine request.
      * @return
      */
    def getSerialResult(lcboProdCategory: String): IndexedSeq[(Long, IProduct)] = {
      val prods = collectProducts(lcboId, category, Store.MaxSampleSize) // take a hit of one go to LCBO, querying by category, no more.
      val permutedIndices = Random.shuffle[Int, IndexedSeq](prods.indices)
      val seq = for (id <- permutedIndices;
                     p = prods(id)) yield (0.toLong, p)
      // we may have 0 inventory, browser should try to finish off that work not web server.
      seq.filter {pair => pair._2.primaryCategory == lcboProdCategory}.take(requestSize)
    }

    // we could get errors going to LCBO, this tryo captures those.
    tryo {
      val lcboProdCategory = LiquorCategory.toPrimaryCategory(category) // transform to the category LCBO uses on product names in results (more or less upper case such as Beer)
      val matchingKeys = productsCacheByCategory.getOrElse(lcboProdCategory, IndexedSeq[Product]()).map(_.lcboId)
      // products are loaded before inventories and we might have none
      asyncLoadCache() // if we never loaded the cache, do it (fast lock free test). Note: useful even if we have product of matching inventory
      val cached = getRequestFromCache(matchingKeys)
      if (cached.nonEmpty) cached
      else getSerialResult(lcboProdCategory)
    }
  }

  // generally has side effect to update database with more up to date content from LCBO's (if different)
  private def loadCache(): Unit = {
    @throws(classOf[SocketTimeoutException])
    @throws(classOf[IOException])
    @throws(classOf[net.liftweb.json.JsonParser.ParseException])
    @throws(classOf[MappingException])
    @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
    @throws(classOf[TruncatedChunkException])  // that's a brutal one.
    def fetchProducts(): Unit = {
      def fetchProductsByStore(): Unit =
        addToCaches(fetchByStore( lcboId)) // ignored if not set meaning take them all

      inTransaction {  // needed
        tryo { fetchProductsByStore() } match {
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading products into cache for '$lcboId' with message $m and exception error $ex")
          case Empty =>
            logger.error(s"Problem loading products into cache for '$lcboId'")
          case _ => ;
        }
      }
    }

    def fetchInventories(): Unit = {
      @throws(classOf[SocketTimeoutException])
      @throws(classOf[IOException])
      @throws(classOf[net.liftweb.json.JsonParser.ParseException])
      @throws(classOf[MappingException])
      @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
      @throws(classOf[TruncatedChunkException])  // that's a brutal one.
      def fetchInventoriesByStore(): Unit = {
        // side effect to MainSchema.inventories cache (managed by Squeryl ORM)

        def stateOfInventory(item: InventoryAsLCBOJson): EnumerationValueType = {
          val pKey = Product.lcboIdToDBId(item.product_id)
          val invOption = for (x <- pKey;
                               inv <- inventoryByProductId.get(x)) yield inv
          (pKey, invOption)  match {
            case (Some(id), None)  => EntityRecordState.New
            case (Some(id), Some(inv)) if inv.isDirty(item) => EntityRecordState.Dirty
            case (Some(id), _) => EntityRecordState.Clean
            case _ => EntityRecordState.Undefined  // on a product we don't yet know, so consider it as undefined so we don't violate FK on products (LCBO makes no guaranty to be consistent here)
          }
        }

        val items = InventoryFetcher.collectItemsOnPages( s"$LcboDomainURL/inventories", Seq("store_id" -> lcboId))
        // partition items into 4 lists, clean (no change), new (to insert) and dirty (to update) and undefined (invalid/unknown product, ref.Integ risk), using neat groupBy
        val inventoriesByState = items.groupBy( stateOfInventory )

        // update in memory our inventories with stale quantity to reflect the trusted LCBO up to date source
        val dirtyInventories = ArrayBuffer[Inventory]()
        for (jsInvs <- inventoriesByState.get(EntityRecordState.Dirty);
             jsInv <- jsInvs;
             prodKey <- Product.lcboIdToDBId(jsInv.product_id);
             dbInv <- inventoryByProductId.get(prodKey))
        { dirtyInventories += dbInv.copyAttributes(jsInv) }

        // create new inventories we didn't know about
        val newInventories = ArrayBuffer[Inventory]()
        for (jsInvs <- inventoriesByState.get(EntityRecordState.New);
             jsInv <- jsInvs;
             prodKey <- Product.lcboIdToDBId(jsInv.product_id))
        { newInventories += new Inventory(pKey, prodKey, jsInv.quantity, jsInv.updated_on, jsInv.is_dead ) }

        // God forbid, we might supply ourselves data that violates composite key. Weed it out!
        def removeCompositeKeyDupes(invs: IndexedSeq[Inventory]): Iterable[Inventory] = {
          invs.groupBy(x => (x.productid, x.storeid)).map{ case (k,v) => v.last }
        }
        // filter the inventories that go to DB to remove dupes and keep a handle of them to help diagnose exceptions should we encounter them.
        val CompKeyFilterNewInventories = removeCompositeKeyDupes(newInventories)
        val CompKeyFilterDirtyInventories = removeCompositeKeyDupes(dirtyInventories)

        try {  // getNextException in catch is what is useful to log (along with the data that led to the exception)
          inTransaction {
            // we refresh just before splitting the inventories into clean, dirty, new classes.
            MainSchema.inventories.update(CompKeyFilterDirtyInventories)
            MainSchema.inventories.insert(CompKeyFilterNewInventories)
            refreshInventories() // always update cache after updating DB
          }
        } catch {
          case se: SQLException =>  // the bad
            // show me the data that caused the problem!
            logger.error(s"SQLException New Invs $CompKeyFilterNewInventories Dirty Invs $CompKeyFilterDirtyInventories")
            logger.error("Code: " + se.getErrorCode)
            logger.error("SqlState: " + se.getSQLState)
            logger.error("Error Message: " + se.getMessage)
            logger.error("NextException:" + se.getNextException)  // the "good". Show me why.
          case e: Exception =>  // the UGLY!
            logger.error("General exception caught: " + e) // we'll pay the price on that one.
        }
      }

      inTransaction { // needed
        tryo { fetchInventoriesByStore() } match {
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading inventories into cache for '$lcboId' with message $m and exception error $ex")
          case Empty =>
            logger.error(s"Problem loading inventories into cache for '$lcboId'")
          case _ => ;
        }
      }
    }

    val doubleFetch = Future {
      fetchProducts() // fetch and then make sure model/Squeryl classes update to DB and their cache synchronously, so we can use their caches.
    } andThen {
      case x => fetchInventories() // similarly for inventories and serialize intentionally because of Ref.  if no exception was thrown
    }
    doubleFetch foreach {
      case m =>
        //We've persisted along the way for each LCBO page ( no need to refresh because we do it each time we go to DB)
        logger.debug(s"loadCache succeeded for ${lcboId}")
        if (emptyInventory) {
          logger.warn(s"got NO product inventory for storeId ${lcboId} !") // No provision for retrying.
        }
    }
    doubleFetch.failed foreach {
      case f =>
        logger.info(s"loadCache explicitly failed for ${lcboId} cause $f")
    }
    logger.info(s"loadCache ended ${lcboId}") // about 15 seconds, likely depends mostly on network/teleco infrastructure
  }

  def asyncLoadCache(): Unit =
    // A kind of guard: Two piggy-backed requests to loadCache for same store will thus ignore second one.
    if (Store.storeProductsLoaded.putIfAbsent(idField.get, Unit).isEmpty) loadCache()

  def refreshInventories(): Unit =
    inTransaction {
      storeProducts.refresh // key for whole inventory caching to work!
      inventoryByProductId ++= getInventories
    }

  def refreshProducts(): Unit =
    inTransaction {
      storeProducts.refresh // key for whole inventory caching and products caching to work!
      productsCache ++= productsByLcboId
      productsCacheByCategory ++= productsByCategory
      inventoryByProductId ++= getInventories
    }
}

object Store extends Store with MetaRecord[Store] with Loggable {
  private val MaxSampleSize = Props.getInt("store.maxSampleSize", 0)

  private val storesCache: concurrent.Map[Long, Store] = TrieMap()  // primary cache
  override val LcboIdsToDBIds: concurrent.Map[Long, Long] = TrieMap() //secondary dependent cache
  override def table(): org.squeryl.Table[Store] = MainSchema.stores

  override def cacheNewItems(items: Iterable[Store]): Unit = {
    super.cacheNewItems(items)
    inTransaction { storesCache.foreach { case (id, s)  => s.refreshProducts() } } // ensure inventories are refreshed INCLUDING on start up.
  }

  private val storeProductsLoaded: concurrent.Map[Long, Unit] = TrieMap()
  // effectively a thread-safe lock-free set, which helps avoiding making repeated requests for cache warm up for a store.

  def availableStores: Set[Long] = storesCache.toMap.keySet
  def lcboIdToDBId(l: Int): Option[Long] = LcboIdsToDBIds.get(l)
  def storeIdToLcboId(s: Long): Option[Long] = storesCache.get(s).map(_.lcboId)
  def getStore(s: Long): Option[IStore] = storesCache.get(s)
  override def getItemByLcboId(id: Long): Option[IStore] =
    for (dbId <- LcboIdsToDBIds.get(id);
         s <- storesCache.get(dbId)) yield s

  override def MaxPerPage = Props.getInt("store.lcboMaxPerPage", 0)

    /* Convert a store to XML */
  implicit def toXml(st: Store): Node =
    <store>{Xml.toXml(st.asJValue)}</store>

  private def getStores(dbStores: Map[Long, Store]): Unit = {
    def collectAllStoresIntoCache(): Box[Unit] = {
      tryo {
        inTransaction {
          val items =  collectItemsOnPages(s"$LcboDomainURL/stores")
          val storesByState: Map[EnumerationValueType, IndexedSeq[Store]] = itemsByState(items)
          // identify the dirty and new stores for batch update and cache them as side effect (automatically)
          val dirtyStores =  storesByState.getOrElse(EntityRecordState.Dirty, IndexedSeq[Store]())
          val newStores = storesByState.getOrElse(EntityRecordState.New, IndexedSeq[Store]()).toIndexedSeq
          updateAndInsert(dirtyStores, newStores)
        }
        logger.debug(s"done loading stores from LCBO")
      }
    }
    collectAllStoresIntoCache() match {
      case Full(m) => ;
      case net.liftweb.common.Failure(m, ex, _) =>
        logger.error(s"Problem loading LCBO stores into cache with message '$m' and exception error '$ex'")
      case Empty =>
        logger.error(s"Problem loading LCBO stores into cache, none found")
    }
  }

  def init(): Unit = {
    logger.info("Store.init start")
    load()
    logger.info("Store.init end")
  }

  override def load(): Unit = {
    inTransaction {
      val items = from(table())(s => select(s))
      cacheNewItems(items)
      asyncGetStores(items.map { s => s.pKey -> s }(breakOut)) // the initial db init is long and synchronous, long because of loading Many-to-Many stateful state, depending on storage data
    }
  }

  def asyncGetStores(x: Map[Long, Store]): Unit = {
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

 def findAll(): Iterable[Store] =
    storesCache.values

}