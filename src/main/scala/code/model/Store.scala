package code.model

import java.io.IOException
import java.net.SocketTimeoutException
import java.sql.SQLException

import scala.annotation.tailrec
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
import code.model.Product.{collectProducts, notifyProducts}

class Store  private() extends IStore with Persistable[Store] with Loader[Store] with LcboJSONCreator[Store] with CreatedUpdated[Store] with Loggable  {

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
    /**
      * Queries LCBO matching category
      * Full URL will be built as follows: http://lcbo.com/products?store_id=<storeId>&q=<category.toLowerCase()>&per_page=<perPage>
      * LCBO allows to specify q as query to specify pattern match on product name (e.g. beer, wine)
      * for pattern match LCBO uses lower case but for actual product category it's upper case, so to make comparisons, we will need to account for that
      * primary_category in catalog or p.primary_category so we need a conversion function to adjust)
      *
      * @param category wine, spirits, and so on
      * @return collection of LCBO products while throwing.
      */
    def productsByStoreCategory(category: String): IndexedSeq[IProduct] = {
      collectProducts(Seq(("store_id", lcboId), ("q", category)), Store.MaxSampleSize)
    }

    def getRequestFromCache(matchingKeys: IndexedSeq[Long]): IndexedSeq[(Long, IProduct)] = {
      // get some random sampling.
      val permutedKeys = Random.shuffle(matchingKeys).take(2 * requestSize)
      // generate double the keys and hope it's enough to find enough products with positive inventory as a result
      // checking quantity in for comprehension below is cost prohibitive.
      val x = for (id <- permutedKeys;
           p <- productsCache.get(id);
           inv <- inventoryByProductId.get(p.id);
           q = inv.quantity if q > 0
      ) yield (q,p)
      x.take(requestSize)
    }

    // side effect
    def getSerialResult: IndexedSeq[(Long, IProduct)] = {
      val prods = productsByStoreCategory(category) // take a hit of one go to LCBO, no more.
      addToCaches(prods)
      val permutedIndices = Random.shuffle[Int, IndexedSeq](prods.indices)
      val seq = for (id <- permutedIndices;
                     lcbo_id = prods(id).lcboId.toInt;
                     p <- productsCache.get(lcbo_id)) yield (0.toLong, p)
      // we may have 0 inventory, browser should try to finish off that work not web server.
      seq.take(requestSize)
      }

    // we could get errors going to LCBO, this tryo captures those.
    tryo {
      val lcboProdCategory = LiquorCategory.toPrimaryCategory(category) // transform to the category LCBO uses on product names in results
      val matchingKeys = productsCacheByCategory.getOrElse(lcboProdCategory, IndexedSeq[Product]()).map(_.lcboId)
      // products are loaded before inventories and we might have none
      asyncLoadCache() // if we never loaded the cache, do it (fast lock free test). Note: useful even if we have product of matching inventory
      val cached = getRequestFromCache(matchingKeys)
      if (cached.nonEmpty) cached
      else getSerialResult
    }
  }

  // generally has side effect to update database with more up to date content from LCBO's (if different)
  private def loadCache(): Unit = {
    def fetchProducts(): Unit = {
      def fetchProductsByStore(): Unit =
        addToCaches(notifyProducts( Seq("store_id" -> lcboId))) // ignored if not set meaning take them all

      inTransaction {  // needed
        tryo { fetchProductsByStore() } match {
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading products into cache for '${lcboId}' with message $m and exception error $ex")
          case Empty =>
            logger.error(s"Problem loading products into cache for '${lcboId}'")
          case _ => ;
        }
      }
    }

    def fetchInventories(): Unit = {
      @throws(classOf[SocketTimeoutException])
      @throws(classOf[IOException])
      @throws(classOf[net.liftweb.json.JsonParser.ParseException])
      @throws(classOf[MappingException])
      def fetchInventoriesByStore(): Unit = {
        // side effect to MainSchema.inventories cache (managed by Squeryl ORM)
        @throws(classOf[net.liftweb.json.MappingException])
        @throws(classOf[net.liftweb.json.JsonParser.ParseException])
        @throws(classOf[java.io.IOException])
        @throws(classOf[java.net.SocketTimeoutException])
        @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
        @tailrec
        def collectInventoriesOnAPage(  urlRoot: String,
                                        pageNo: Int,
                                        params: Seq[(String, Any)]): Unit = {
          def stateOfInventory(item: InventoryAsLCBOJson): EnumerationValueType = {
            val prodId = Product.lcboIdToDBId(item.product_id)
            val invOption = for (x <- prodId;
                                 inv <- inventoryByProductId.get(x)) yield inv

            (prodId, invOption)  match {
              case (Some(id), None)  => EntityRecordState.New
              case (Some(id), Some(inv)) if inv.dirty_?(item) => EntityRecordState.Dirty
              case (Some(id), _) => EntityRecordState.Clean
              case _ => EntityRecordState.Undefined  // on a product we don't yet know, so consider it as undefined so we don't violate FK on products (LCBO makes no guaranty to be consistent here)
            }
          }

          // specify the URI for the LCBO api url for liquor selection
          val fullParams: Seq[(String, Any)] = params ++ Seq(("per_page", Store.MaxPerPage), ("page", pageNo))
          val uri = buildUrl(urlRoot, fullParams)
          val pageContent = get(uri)
          val jsonRoot = parse(pageContent)
          val itemNodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
          val items = (for (p <- itemNodes) yield p.extract[InventoryAsLCBOJson]).filter(!_.is_dead)

          // partition items into 4 lists, clean (no change), new (to insert) and dirty (to update) and undefined (invalid/unknown product, ref.Integ risk), using neat groupBy
          val storeProductsByState: Map[EnumerationValueType, IndexedSeq[InventoryAsLCBOJson]] = items.toIndexedSeq.groupBy( stateOfInventory )

          val dirtyInventories = ArrayBuffer[Inventory]()
          for (jsInvs <- storeProductsByState.get(EntityRecordState.Dirty);
               jsInv <- jsInvs;
               dbId <- Product.lcboIdToDBId(jsInv.product_id);
               dbInv <- inventoryByProductId.get(dbId))
          {
            val updInv = dbInv.copyAttributes(jsInv)
            dirtyInventories += updInv
          }

          val newInventories = ArrayBuffer[Inventory]()
          for (jsInvs <- storeProductsByState.get(EntityRecordState.New);
               jsInv <- jsInvs;
               dbId <- Product.lcboIdToDBId(jsInv.product_id))
          {
            val newInv = new Inventory(idField.get, dbId, jsInv.quantity, jsInv.updated_on, jsInv.is_dead )
            newInventories += newInv
          }

          // God forbid, we might supply ourselves data that violates composite key. Weed it out!
          def removeCompositeKeyDupes(invs: IndexedSeq[Inventory]): Iterable[Inventory] = {
            invs.groupBy(x => (x.productid, x.storeid)).map{ case (k,v) => v.last }
          }
          val CompKeyFilterNewInventories = removeCompositeKeyDupes(newInventories)
          val CompKeyFilterDirtyInventories = removeCompositeKeyDupes(dirtyInventories.toIndexedSeq)

          try {  // getNextException in catch is what is useful to log (along with the data that led to the exception)
            inTransaction {
              // we refresh just before splitting the inventories into clean, dirty, new classes.
              MainSchema.inventories.update(CompKeyFilterDirtyInventories)
              MainSchema.inventories.insert(CompKeyFilterNewInventories)
              refreshInventories() // always update cache after updating DB
            }
          } catch {
            case se: SQLException =>  // the bad
              logger.error(s"SQLException New Invs $CompKeyFilterNewInventories Dirty Invs $CompKeyFilterDirtyInventories")
              logger.error("Code: " + se.getErrorCode)
              logger.error("SqlState: " + se.getSQLState)
              logger.error("Error Message: " + se.getMessage)
              logger.error("NextException:" + se.getNextException)  // the "good".
            case e: Exception =>  // the UGLY!
              logger.error("General exception caught: " + e)
          }

          if (isFinalPage(jsonRoot, pageNo=pageNo)) {
            logger.info(uri) // show what's going after several pages of background requests
            return
          }
          collectInventoriesOnAPage( urlRoot, pageNo + 1, params) // recurse to cache all we can
        }
        collectInventoriesOnAPage(s"$LcboDomainURL/inventories", 1, params=Seq(("store_id", lcboId)))
      }
      inTransaction { // needed
        tryo { fetchInventoriesByStore() } match {
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading inventories into cache for '${lcboId}' with message $m and exception error $ex")
          case Empty =>
            logger.error(s"Problem loading inventories into cache for '${lcboId}'")
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
      storeProducts.refresh // key for whole inventory caching to work!
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
  def getStoreByLcboId(id: Long): Option[Store] =
    for (dbid <- Product.lcboIdToDBId(id);
          s <- storesCache.get(dbid)) yield s

  def MaxPerPage = Props.getInt("store.lcboMaxPerPage", 0)

    /* Convert a store to XML */
  implicit def toXml(st: Store): Node =
    <store>{Xml.toXml(st.asJValue)}</store>

  private def getStores(dbStores: Map[Long, Store]): Unit = {
    def collectAllStoresIntoCache(): Box[Unit] = {
      // collects stores individually from LCBO REST as Store on all pages starting from a pageNo.
      @tailrec
      def collectStoresOnAPage(urlRoot: String,
                               pageNo: Int): Unit = {
        val (items, jsonRoot, uri) = extractLcboItems(urlRoot, Seq(), pageNo=pageNo, MaxPerPage)
        // partition pageStoreSeq into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
        val storesByState: Map[EnumerationValueType, IndexedSeq[Store]] = items.groupBy {
          s =>  ( getStoreByLcboId(s.lcboId), s) match {
            case (None, _) => EntityRecordState.New
            case (Some(store), item) if store.isDirty(item) => EntityRecordState.Dirty
            case (_ , _) => EntityRecordState.Clean  // or decided not to handle such as stores "out of bound" that we won't cache.
          }
        }

        // identify the dirty and new stores for batch update and cache them as side effect (automatically)
        val dirtyStores =  storesByState.getOrElse(EntityRecordState.Dirty, IndexedSeq[Store]())
        val newStores = storesByState.getOrElse(EntityRecordState.New, IndexedSeq[Store]()).toIndexedSeq
        updateAndInsert(dirtyStores, newStores)

        if (isFinalPage(jsonRoot, pageNo=pageNo)) {
          logger.info(uri)
          return
        } // log only last one to be less verbose
        collectStoresOnAPage(urlRoot, pageNo + 1) // fetch on next page
      }

      tryo {
        // gather stores on this page (url) and recursively to following pages
        inTransaction { collectStoresOnAPage(urlRoot=s"$LcboDomainURL/stores", pageNo=1) }
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
      asyncGetStores(items.map { s => s.idField.get -> s }(breakOut)) // the initial db init is long and synchronous, long because of loading Many-to-Many stateful state, depending on storage data
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