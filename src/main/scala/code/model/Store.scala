package code.model

import java.io.IOException

import scala.annotation.tailrec
import scala.collection.{Iterable, Set, Map, concurrent,breakOut}
import scala.language.implicitConversions
import scala.util.Random
import scala.xml.Node
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import net.liftweb.common.{Full, Empty, Box, Loggable}
import net.liftweb.json._
import net.liftweb.json.JsonParser.{ParseException, parse}
import net.liftweb.util.Helpers.tryo
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.record.field._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props

import org.squeryl.annotations._

import code.model.Product._
import code.Rest.pagerRestClient
import MainSchema._

/**
  * Created by philippederome on 15-11-01.
  * This is captured from JSON parsing.
  */
case class StoreAsLCBOJson (id: Int = 0,
                 is_dead: Boolean = true,
                 latitude: Double = 0.0,
                 longitude: Double = 0.0,
                 name: String = "",
                 address_line_1: String = "",
                 city: String = "",
                 distance_in_meters: Int = 0) {
  def this(s: Store) = this(s.lcbo_id.get, false, s.latitude.get, s.longitude.get, s.name.get, s.address_line_1.get, s.city.get  )

  // intentional aliasing allowing more standard naming convention.
  def isDead = is_dead

  override def toString = s"$id, name: $name, Address: $address_line_1, city: $city, distance is:$distanceInKMs"

  // intentional change of scale from metres to kilometres, using String representation instead of integer and keeping 3 decimals (0.335 ml for beer)
  def distanceInKMs: String = {
    val d = distance_in_meters / 1000.0
    f"$d%1.1f KM(s)"
  }

  /**
    *
    * @return an ordered list of pairs of values (label and value), representing most of the interesting data of the product
    */
  def createProductElemVals: IndexedSeq[Attribute] =
    (Attribute("Name: ", name) ::
     Attribute ("Primary Address: ", address_line_1) ::
     Attribute("City: ", city) ::
     Attribute("Your Distance: ", distanceInKMs) ::
     Attribute("Latitude: ", latitude.toString) ::
     Attribute("Longitude: ", longitude.toString) ::
     Nil).filterNot{ attr =>  attr.value == "null" || attr.value.isEmpty }.toVector

}

object StoreAsLCBOJson {
  private implicit val formats = net.liftweb.json.DefaultFormats

  def apply(s: Store) = new StoreAsLCBOJson(s)

  /**
    * Convert a store to XML
    */
  implicit def toXml(st: StoreAsLCBOJson): Node =
    <store>{Xml.toXml(st)}</store>

  /**
    * Convert the store to JSON format.  This is
    * implicit and in the companion object, so
    * a Store can be returned easily from a JSON call
    */
  implicit def toJson(st: StoreAsLCBOJson): JValue =
    Extraction.decompose(st)

}

case class PlainStoreAsLCBOJson (id: Int = 0,
                                 is_dead: Boolean = true,
                                 latitude: Double = 0.0,
                                 longitude: Double = 0.0,
                                 name: String = "",
                                 address_line_1: String = "",
                                 city: String = "") {

  def getStore(dbStores: Map[Int, Store]) = dbStores.get(id)
}

class Store private() extends Record[Store] with KeyedRecord[Long] with CreatedUpdated[Store]  {
  lazy val userStores = MainSchema.storeToUserStores.left(this)
  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id
  val lcbo_id = new IntField(this) // we don't share same PK as LCBO!
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

  def meta = Store

  def copyAttributes(my: Store, s: PlainStoreAsLCBOJson): Store = {
    my.is_dead.set(s.is_dead)
    my.address_line_1.set(s.address_line_1)
    my
  }

  def synchUp(s: PlainStoreAsLCBOJson): Unit = {
    def copyAttributes(s: PlainStoreAsLCBOJson): Unit = {
      is_dead.set(s.is_dead)
      address_line_1.set(s.address_line_1)
    }

    if (isDirty(s)) {
      copyAttributes(s)
      updated.set(updated.defaultValue)
      this.update  // Active Record pattern
    }
  }

  def isDirty(s: PlainStoreAsLCBOJson): Boolean = {
    is_dead.get != s.is_dead ||
    address_line_1.get != s.address_line_1
  }
}

object Store extends Store with MetaRecord[Store] with pagerRestClient with Loggable {
  private implicit val formats = net.liftweb.json.DefaultFormats
  private val PositiveInventoryIterations = Props.getInt("store.PositiveInventoryIterations",  10)
  private val MaxSampleSize = Props.getInt("store.maxSampleSize", 0)
  private val DBBatchSize = Props.getInt("store.DBBatchSize", 1)
  private val storeLoadAll = Props.getBool("store.loadAll", false)
  private val storeLoadBatchSize = Props.getInt("store.loadBatchSize", 10)

  private val storeCategoriesProductsCache: concurrent.Map[(Int, String), Set[Int]] = TrieMap() // give set of available productIds by store+category
  private val storeProductsLoaded: concurrent.Map[Int, Unit] = TrieMap() // effectively a thread-safe lock-free set, which helps control access to storeCategoriesProductsCache.
  // would play a role if user selects a store from a map. A bit of an exercise for caching and threading for now.
  private val storesCache: concurrent.Map[Int, Store] = TrieMap[Int, Store]()


  override def MaxPerPage = Props.getInt("store.lcboMaxPerPage", 0)

  override def MinPerPage = Props.getInt("store.lcboMinPerPage", 0)

  @volatile
  var dummy: Any = _
  def timed[T](body: =>T): Double = {
    val start = System.nanoTime
    dummy = body
    val end = System.nanoTime
    ((end - start) / 1000) / 1000.0
  }

  def fetchItems(state: EntityRecordState,
                 storesByState: Map[EntityRecordState, IndexedSeq[PlainStoreAsLCBOJson]],
                 f: PlainStoreAsLCBOJson => Option[Store] ): IndexedSeq[Store] =
    storesByState.getOrElse(state, Vector()).flatMap{ f(_)}

  def init() = {

    def asyncLoadStores(storeIds: Iterable[Int]): Unit = {
      def dbLoadCache(storeIds: Iterable[Int]): Future[ Unit] = {
        val fut = Future { storeIds.foreach(loadCache)}
        fut foreach {
          case m =>
            logger.trace(s"asyncLoadStores succeeded for ${storeIds.mkString(" ")}")
            storeIds.foreach { s =>
              StoreProduct.emptyInventoryForStore(s)
              if (StoreProduct.emptyInventoryForStore(s)) {
                logger.warn(s"got NO product inventory for store $s !") // could trigger a retry later on.
              }
            }
        }
        fut.failed foreach {
          case f =>
          logger.info(s"asyncLoadStores explicitly failed for ${storeIds.mkString(":")} cause $f")
        }
        fut
      }

      storeIds.grouped(storeLoadBatchSize).foreach { dbLoadCache }
    }

    def getStores(dbStores: Map[Int, Store]): Map[Int, Store] = {
      def synchronizeData(dbStores: Map[Int, Store]): Box[Map[Int, Store]] = {
        // collects stores individually from LCBO REST as PlainStoreAsLCBOJson on as many pages as required.
        // we declare types fairly often in the following because it's not trivial to follow otherwise
        @tailrec
        def collectStoresOnAPage(dbStores: Map[Int, Store],
                                 accumItems: Map[EntityRecordState, IndexedSeq[Store]],
                                 urlRoot: String,
                                 pageNo: Int): Map[EntityRecordState, IndexedSeq[Store]] = {
          val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo)
          logger.info(uri)
          val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
          val jsonRoot = parse(pageContent) // fyi: throws ParseException
          val itemNodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
          // get all the stores from JSON itemNodes, extract them and map them to usable Store class after synching it with our view of same record in database.
          val pageStores = {for (p <- itemNodes) yield p.extractOpt[PlainStoreAsLCBOJson]}.flatten

          // partition pageStoreSeq into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
          val storesByState: Map[EntityRecordState, IndexedSeq[PlainStoreAsLCBOJson]] = pageStores.groupBy {
            s => (dbStores.get(s.id), s) match {
              case (None, _)  => New
              case (Some(store), lcboStore) if store.isDirty(lcboStore) => Dirty
              case (_ , _) => Clean
            }
          }

          val cleanStores = accumItems(Clean) ++ fetchItems(Clean, storesByState, s => s.getStore(dbStores))
          val dirtyStores = accumItems(Dirty) ++ fetchItems(Dirty, storesByState, s => s.getStore(dbStores).map(copyAttributes(_, s) ))
          val newStores = accumItems(New) ++ storesByState.getOrElse(New, Nil).map{ create }

          // after preliminaries, get the map of stores indexed properly by state that we need having accumulated over the pages so far.
          val revisedAccumItems: Map[EntityRecordState, IndexedSeq[Store]] = Map(New -> newStores, Dirty -> dirtyStores, Clean -> cleanStores)

          val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)

          if (pageStores.isEmpty || isFinalPage) return revisedAccumItems  // no need to look at more pages

          collectStoresOnAPage(
            dbStores,
            revisedAccumItems,
            urlRoot,
            pageNo + 1) // union of this page with next page when we are asked for a full sample
        }

        // we'd like the is_dead ones as well to update state (but apparently you have to query for it explicitly!?!?)
        val url = s"$LcboDomainURL/stores?"
        tryo {
          // gather stores on this page (url) with classification as to whether they are new, dirty or clean
          val initMap = Map[EntityRecordState, IndexedSeq[Store]](New -> Vector(), Dirty -> Vector(), Clean -> Vector())
          val lcboStores = collectStoresOnAPage(dbStores, initMap, url, 1)
          logger.trace(s"done loading to LCBO")

          // identify the dirty and new stores for batch update and then retain all of them as the requested set
          inTransaction {
            // for the activity on separate thread for synchronizeData
            // batch update the database now.
            updateStores(lcboStores(Dirty))
            insertStores(lcboStores(New))
          }
          lcboStores.values.flatten.map(s => s.lcbo_id.get -> s)(breakOut) // flatten the 3 lists and then build a map from the stores keyed by lcbo_id.
        }
      }

      import net.liftweb.common.Failure
      synchronizeData( dbStores) match {
          case Full(m) => m
          case Failure(m, ex, _) =>
            logger.error(s"Problem loading LCBO stores into cache with message '$m' and exception error '$ex'")
            dbStores
          case Empty =>
            logger.error(s"Problem loading LCBO stores into cache, none found")
            dbStores
      }
    }

    logger.trace(s"Store.init start")
    Product.init()
    // load all stores from DB for navigation and synch with LCBO for possible delta (small set so we can afford synching, plus it's done async way)
    val dbStores: Map[Int, Store] = inTransaction { stores.map(s => s.lcbo_id.get -> s)(breakOut) }// queries full store table and throw it into map
    StoreProduct.init(dbStores.keys.max)
    // warm up from our database known products by storeId and category, later on load all stores for navigation.
    // declaration is to verify we already support indexing so we can group by efficiently
    StoreProduct.getProductIdsByStore.foreach {
      case (storeId, seqOfProductIds) =>
        val seqOfProducts = seqOfProductIds.flatMap ( Product getProduct )
        val productsByCategory = seqOfProducts.groupBy(_.primary_category.get) // partition the products of a store by category (make sure to get a vector first)
        productsByCategory.foreach { case (category, productsIter) =>
          // we're first setter of the cache, so we don't need to append if already in cache.
          val dbProductIds = productsIter.map(_.lcbo_id.get).toSet
          if (storeCategoriesProductsCache.putIfAbsent((storeId, category), dbProductIds).isDefined)   // restrict the products to simply productId (lcbo_id) and put  in cache lock-free
            storeCategoriesProductsCache((storeId, category)) ++= dbProductIds
        }
    }


    def isPopular(storeId: Int): Boolean = {
      storeLoadAll || //This does about 180-200 stores per hour out of 600 and ultimately chokes on GC. Beware!
      StoreProduct.storeIdsWithCachedProducts.contains(storeId)
    }
    val res = getStores(dbStores)
    storesCache ++= res
    val (stockedStores, emptyStores) = res.keys.partition( StoreProduct.hasCachedProducts)
    logger.debug(s"stocked ${stockedStores}")
   // logger.debug(s"stocked ${stockedStores} empty ${emptyStores}")

    // asyncLoadStores(emptyStores ++ stockedStores)
    asyncLoadStores(res.keys.filter{isPopular})
  logger.trace("Store.init end")
  }

  /**
    * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
    * Select a random product that matches the parameters subject to a max sample size.
    *
    * @param store a String representing a numeric code of a LCBO store
    * @param category a String such as beer, wine, mostly matching primary_category at LCBO, or an asset category.
    * @return
    */
  def recommend(storeId: Int, category: String, requestSize: Int): Box[Iterable[(Int, Product)]] = {
    def randomSampler(prodKeys: IndexedSeq[Int]) = {
      val lcboids = Random.shuffle(prodKeys)
      // generate double the keys and hope it's enough to have nearly no 0 inventory as a result
      val seq =
        for ( lcbo_id <- lcboids;
           p <- Product.getProduct(lcbo_id);
           qty <- StoreProduct.getStoreProductQuantity(storeId, lcbo_id) if qty > 0 ) yield (qty, p)
      seq.take(requestSize)
    }

    // note: cacheSuccess (top level code) grabs a lock
    def cacheSuccess(storeId: Int, category: String): Option[Iterable[(Int, Product)]] = {
      // note: we never delete from cache, so we don't worry about data disappearing in a potential race condition
      if (storeCategoriesProductsCache.contains((storeId, category)) && storeCategoriesProductsCache((storeId, category)).nonEmpty)
        Option(randomSampler(storeCategoriesProductsCache((storeId, category)).toVector))
      else None
    }

    tryo { // we could get errors going to LCBO, this tryo captures those.
      cacheSuccess(storeId, LiquorCategory.toPrimaryCategory(category)).map { identity} getOrElse {
        logger.warn(s"recommend cache miss for $storeId") // don't try to load it asynchronously as that's start up job to get going with it.
        Future {loadCache(storeId)}
        val prods = productsByStoreCategory(MaxSampleSize, storeId, category) // take a hit of one go to LCBO, no more.
        val indices = Random.shuffle[Int, IndexedSeq](0 until prods.size )
        // generate double the keys and hope it's enough to have nearly no 0 inventory as a result
        val seq = {
          if (StoreProduct.hasCachedProducts(storeId)) // use inventory cache if it has anything useful to say
            for (idx <- indices;
               p = prods(idx);
               lcbo_id = p.lcbo_id.get;
               qty <- StoreProduct.getStoreProductQuantity(storeId, lcbo_id) if qty > 0) yield (qty, p)
          else // just use 0 as placeholder for inventory for now as we don't have this info yet.
            for (idx <- indices;
                 p = prods(idx);
                 lcbo_id = p.lcbo_id.get) yield (0, p)
        }
        seq.take(requestSize)
      }
    }
  }

  def create(s: PlainStoreAsLCBOJson): Store = {
    // store in same format as received by provider so that un-serializing if required will be same logic. This boiler-plate code seems crazy (not DRY at all)...
    createRecord.
      lcbo_id(s.id).
      name(s.name).
      is_dead(s.is_dead).
      address_line_1(s.address_line_1).
      city(s.city).
      latitude(s.latitude).
      longitude(s.longitude)
  }

  def findAll(): Iterable[StoreAsLCBOJson] =
    storesCache.values.map( StoreAsLCBOJson(_))

  def updateStores(myStores: Iterable[Store]) =
    myStores.grouped(DBBatchSize).
      foreach{ stores.update }

  def insertStores( myStores: Iterable[Store]) =
    myStores.grouped(DBBatchSize).
      foreach{ stores.insert }

  // may have side effect to update database with more up to date from LCBO's content (if different)
  // lock free. For correctness, we use putIfAbsent to get some atomicity when we do complex read-write at once (complex linearizable operations).
  // For now, we call this lazily for first user having an interaction with a particular store and not before.
  // heavy long lasting, for now no thread inside, but clients call this asynchronously
  def loadCache(storeId: Int) = {

    def getProductsOfStartPage(dbProducts: Map[Int, Product], allDbProductIds: Set[Int]): Iterable[Product] = {
      def loadAll(dbProducts: Map[Int, Product], requestSize: Int, storeId: Int, page: Int): Box[Map[EntityRecordState, IndexedSeq[Product]]] =
        tryo { getProductsByStore(dbProducts, requestSize, storeId, page) }

      inTransaction {
        loadAll(dbProducts, productCacheSize, storeId, 1) match {
          case Full(m) => // Used to be 10 seconds to get here after last URL query, down to 0.046 sec
            for ((k, v) <- m) {
              // we don't want to have two clients taking responsibility to update database for synchronization
              Some(k) collect {
                // Clean is intentionally ignored
                case New =>
                  Product.insertProducts(v) // insert to DB those we didn't get in our query to obtain allDbProducts
                case Dirty =>
                  Product.updateProducts(v) // discard inventory that we don't need. This is just conveniently realizing our product is out of date and needs touched up to DB.
              }
            }
            // 5 seconds per thread (pre Jan 23 with individual db writes). Now 1.5 secs for a total of 2-3 secs, compared to at least 15 secs.
            m.values.flatten // flatten the 3 lists
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading products into cache for '$storeId' with message $m and exception error $ex")
            List[Product]()
          case Empty =>
            logger.error(s"Problem loading products into cache for '$storeId'")
           List[Product]()
        }
      }
    }

    def getStoreProductsOfStartPage(dbStoreProducts: Map[(Int, Int), StoreProduct], allDbProductIds: Set[Int]): Vector[StoreProduct] = {
      def loadAllStoreProducts(dbStoreProducts: Map[(Int, Int), StoreProduct], requestSize: Int, storeId: Int, page: Int): Box[Map[EntityRecordState, IndexedSeq[StoreProduct]]] =
        tryo { storeProductByStore(dbStoreProducts, requestSize, storeId, page) }

      inTransaction {
        loadAllStoreProducts(dbStoreProducts, productCacheSize, storeId, 1) match {
          case Full(m) => // Used to be 10 seconds to get here after last URL query, down to 0.046 sec
            for ((k, v) <- m) {
              Some(k) collect {
                // Clean is intentionally ignored
                case New =>
                  StoreProduct.insertStoreProducts(v)
                case Dirty =>
                  StoreProduct.updateStoreProducts(v)
              }
            }
            // 5 seconds per thread (pre Jan 23 with individual db writes). Now 1.5 secs for a total of 2-3 secs, compared to at least 15 secs.
            m.values.flatten.toVector // flatten the 3 lists
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading inventories into cache for '$storeId' with message $m and exception error $ex")
            Vector[StoreProduct]()
          case Empty =>
            logger.error(s"Problem loading inventories into cache for '$storeId'")
            Vector[StoreProduct]()
        }
      }
    }

    val allDbProductIds = Product.cachedProductIds
    val dbProducts = Product.getProducts
    val dbStoreProducts = StoreProduct.getInventories
    def fetchInventories(dbStoreProducts: Map[(Int, Int), StoreProduct],
                         allDbProductIds: Set[Int]): Unit = {
      val allTheStoreInventories = getStoreProductsOfStartPage(dbStoreProducts, allDbProductIds)
      StoreProduct.update(storeId, allTheStoreInventories )
    }

    def fetchProducts(): Unit= {
      // evaluate the maps of product in parallel, tracking them by product id (lcbo_id) as key but in order to be able to cache them by category when aggregating results back.
      val prods = getProductsOfStartPage(dbProducts, allDbProductIds)
      val productsByCategory = prods.toVector.groupBy(_.primary_category.get) // groupBy is a dandy!
      productsByCategory.foreach {
        case (category, fetchedProducts) =>
          val storeCatKey = (storeId, category) // construct the key we need, i.e. grab the storeId
          val newProductIdsSet = fetchedProducts.map(_.lcbo_id.get).toSet // project the products to only the productId
          if (storeCategoriesProductsCache.putIfAbsent(storeCatKey, newProductIdsSet).isDefined) {
            // if was empty, we just initialized it atomically with set(id) but if there was something, just add to set below
            storeCategoriesProductsCache(storeCatKey) ++= newProductIdsSet
          }
      }
      Product.update(prods) // refresh product cache
      fetchInventories(dbStoreProducts, allDbProductIds)
    }

    logger.trace(s"loadCache start $storeId") // 30 seconds from last LCBO query to completing the cache update (Jan 23). Needs to be better.
    if ( storeProductsLoaded.putIfAbsent(storeId, Unit).isEmpty) {
      // A kind of guard: Two piggy-backed requests to loadCache for same store will thus ignore second one.
      // Slightly unwanted consequence is that clients need to test for empty set and not assume it's non empty.
      // we impose referential integrity so we MUST get products and build on that to get inventories that refer to products
      fetchProducts()
    }
  }

  import java.net.SocketTimeoutException

  // for reflection and generating documentation

  private final def getSingleStore( uri: String): PlainStoreAsLCBOJson = {
    logger.debug(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    (parse(pageContent) \ "result").extract[PlainStoreAsLCBOJson] // more throws
  }

  /**
    * Queries LCBO matching category and storeId for a sample size as specified by client, with category considered optional, though not tested when optional.
    * Full URL will be built as follows: http://lcbo.com/products?store_id=<storeId>&q=<category.toLowerCase()>&per_page=<perPage>
    * LCBO allows to specify q as query to specify pattern match on product name (e.g. beer, wine)
    * for pattern match LCBO uses lower case but for actual product category it's upper case, so to make comparisons, we will need to account for that
    * primary_category in catalog or p.primary_category so we need a conversion function to adjust)
    *
    * @param requiredSize upper bound on #items we need. Attempt to match it if enough supply is available.
    * @param store id  of Store at LCBO
    * @param category wine, spirits, and so on
    * @return collection of LCBO products while throwing.
    * @throws SocketTimeoutException timeout reached
    * @throws IOException I/O issue
    * @throws ParseException parse issue
    * @throws MappingException, etc
    */
  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  private def productsByStoreCategory(requiredSize: Int, storeId: Long, category: String): IndexedSeq[Product] = {
    if (requiredSize <= 0) Vector()
    else {
      val url = s"$LcboDomainURL/products?store_id=$storeId" + additionalParam("q", category) // does not handle first one such as storeId, which is artificially mandatory
      val filter = { p: ProductAsLCBOJson => p.primary_category == LiquorCategory.toPrimaryCategory(category) &&
        !p.is_discontinued
      } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers
      val items = collectItemsOnAPage(
        Map[Int, Product](),  // suppress reconciliation with database, just trust LCBO now. Passing this empty cache to reconcile with will yield products classified as New.
        Map[EntityRecordState, IndexedSeq[Product]](New -> Vector(), Dirty -> Vector(), Clean -> Vector()),
        url,
        requiredSize,
        pageNo = 1,
        pageDelta = 1,
        filter)
      items.values.take(requiredSize).flatten.toVector // we don't care about Clean/New/Dirty state here so flatten values.
    }
  }

  // Fetch both the original product as case class JSON-like from LCBO
  // as well as the DB Record we'd associate it with as Product and key that pair
  // with LCBO product id. The list is generated by a query scoped by the store id.
  // We also make a somewhat arbitrary assumption that discontinued products are of zero interest
  // So we use that filter on REST API call to LCBO.
  // The original product is retrieved because it has store-sensitive context such as inventory
  // which we want to inform user of but that does not belong to the "pure" Product itself.
  // The association from JSON-like LCBO product type to our db type is done with fetchSynched.
  // This picks up the product from database and brings it up to date.
  // Future work:
  //     do update in series not one-by-one for scalability.
  //     Make use of a product cache we already have, so don't go to database unconditionally naively.
  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  private def getProductsByStore(dbProducts: Map[Int, Product],
                                 requiredSize: Int,
                                 store: Long,
                                 page: Int): Map[EntityRecordState, IndexedSeq[Product]] = {
    if (requiredSize <= 0) Map()
    else {
      val url = s"$LcboDomainURL/products?store_id=$store"
      val filter = { p: ProductAsLCBOJson => !p.is_discontinued } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers
      val initMap = Map[EntityRecordState, IndexedSeq[Product]] (New -> Vector(), Dirty -> Vector(), Clean -> Vector())

      collectItemsOnAPage(
        dbProducts,
        initMap,
        url,
        requiredSize,
        page, // programmer client is assumed to know we use this as a page.
        prodLoadWorkers,
        filter).take(requiredSize)
    }
  }

  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  private def storeProductByStore(dbStoreProducts: Map[(Int, Int), StoreProduct],
                                 requiredSize: Int,
                                 store: Int,
                                 page: Int): Map[EntityRecordState, IndexedSeq[StoreProduct]] = {
    @throws(classOf[net.liftweb.json.MappingException])
    @throws(classOf[net.liftweb.json.JsonParser.ParseException])
    @throws(classOf[java.io.IOException])
    @throws(classOf[java.net.SocketTimeoutException])
    @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
    @tailrec
    def collectStoreProductsOnAPage(dbStoreProducts: Map[(Int, Int), StoreProduct],
                                                  accumItems: Map[EntityRecordState, IndexedSeq[StoreProduct]],
                                                  urlRoot: String,
                                                  requiredSize: Int,
                                                  pageNo: Int,
                                                  pageDelta: Int = 1,
                                                  myFilter: InventoryAsLCBOJson => Boolean = { sp: InventoryAsLCBOJson => true }): Map[EntityRecordState, IndexedSeq[StoreProduct]] = {
      def nextPage = pageNo + prodLoadWorkers
      def fetchItems(state: EntityRecordState,
                     mapVector: Map[EntityRecordState, IndexedSeq[InventoryAsLCBOJson]],
                     f: InventoryAsLCBOJson => Option[StoreProduct] ): IndexedSeq[StoreProduct] = {
        mapVector.getOrElse(state, Vector()).flatMap { p => f(p) } // remove the Option in Option[Product]
      }

      if (requiredSize <= 0) return accumItems
      // specify the URI for the LCBO api url for liquor selection
      val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo) // get as many as possible on a page because we could have few matches.
      logger.info(uri)
      val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
      val jsonRoot = parse(pageContent) // fyi: throws ParseException
      val itemNodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
      val items = (for (p <- itemNodes) yield p.extract[InventoryAsLCBOJson].removeNulls).filter(myFilter)  // LCBO sends us poisoned useless nulls that we need to filter for DB (filter them right away).
      val outstandingSize = requiredSize - items.size

      // Collects into our list of products the attributes we care about (extract[Product]). Then filter out unwanted data.
      // fyi: throws Mapping exception.
      //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
      val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
      val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)

      // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
      val storeProductsByState: Map[EntityRecordState, IndexedSeq[InventoryAsLCBOJson]] = items.groupBy {
        i => dbStoreProducts.get(store, i.product_id) match {
          case None  => New
          case Some(quantity) if i.quantity != quantity => Dirty
          case _ => Clean
        }
      }

      val cleanProducts = accumItems(Clean) ++ fetchItems(Clean, storeProductsByState, {inv => dbStoreProducts.get(inv.store_id, inv.product_id )})
      val dirtyProducts = accumItems(Dirty) ++ fetchItems(Dirty, storeProductsByState, {inv => dbStoreProducts.get(inv.store_id, inv.product_id ).map { _.copyAttributes( inv)} } )
      val newProducts = accumItems(New) ++ storeProductsByState.getOrElse(New, Nil).map { sp => StoreProduct.create(sp) }
      val revisedAccumItems: Map[EntityRecordState, IndexedSeq[StoreProduct]] = Map(New -> newProducts, Dirty -> dirtyProducts, Clean -> cleanProducts)

      if (outstandingSize <= 0 || isFinalPage || totalPages < nextPage) return revisedAccumItems
      // Deem as last page only if  LCBO tells us it's final page or we evaluate next page won't have any (when we gap due to parallelism).
      // Similarly having reached our required size,we can stop.

      // tail recursion enforced.
      collectStoreProductsOnAPage(
        dbStoreProducts,
        revisedAccumItems,
        urlRoot,
        outstandingSize,
        nextPage,
        pageDelta,
        myFilter) // union of this page with next page when we are asked for a full sample
    }

    if (requiredSize <= 0) Map()
    else {
      val url = s"$LcboDomainURL/inventories?store_id=$store"
      val filter = { p: InventoryAsLCBOJson => !p.is_dead } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers
      val initMap = Map[EntityRecordState, IndexedSeq[StoreProduct]] (New -> Vector(), Dirty -> Vector(), Clean -> Vector())

      collectStoreProductsOnAPage(
        dbStoreProducts,
        initMap,
        url,
        requiredSize,
        page, // programmer client is assumed to know we use this as a page.
        prodLoadWorkers,
        filter).take(requiredSize)
    }
  }

  /**
    * LCBO client JSON query handler. So naturally, the code is specifically written with the structure of LCBO documents in mind, with tokens as is.
    * For Liftweb JSON extraction after parse,
    *
    * @see https://github.com/lift/lift/tree/master/framework/lift-base/lift-json/
    *      don't go to more pages than user implicitly requests via requiredSize that should not be exceeded.
    *      Would Streams collection be handy for paging here? Depends on consumption usage perhaps.
    *      Uses tail recursion.
    * @param accumItems accumulator to facilitate tail recursion
    * @param urlRoot a LCBO product query without the details of paging, which we handle here
    * @param requiredSize required size of products that are asked for. May get less if there are fewer matches, but will not go above that size.
    * @param pageNo client calls this with value 1 (initial page), recursion increments it, designates the pageno for LCBO JSON data when data fits on several pages
    * @param myFilter client's filter that can be applied as we process the data before mapping/extracting it out to client data.
    *                 In principle, should be faster when user filters reject many values, but no empirical evidence here.
    * @return a vector of product items matching the query and size constraint, though we may go a bit over the size by multiple of page sizes.
    * @throws java.net.SocketTimeoutException timeout is reached, slow connection
    * @throws java.io.IOException I/O issue
    * @throws net.liftweb.json.JsonParser.ParseException parse problem
    * @throws net.liftweb.json.MappingException our case class does not match JSon object from API
    *
    */
  @throws(classOf[net.liftweb.json.MappingException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @tailrec
  private final def collectItemsOnAPage(dbProducts: Map[Int, Product],
                                        accumItems: Map[EntityRecordState, IndexedSeq[Product]],
                                        urlRoot: String,
                                        requiredSize: Int,
                                        pageNo: Int,
                                        pageDelta: Int = 1,
                                        myFilter: ProductAsLCBOJson => Boolean = { p: ProductAsLCBOJson => true }): Map[EntityRecordState, IndexedSeq[Product]] = {
    def nextPage = pageNo + prodLoadWorkers
    def fetchItems(state: EntityRecordState,
                   productsByState: Map[EntityRecordState, IndexedSeq[ProductAsLCBOJson]],
                   f: ProductAsLCBOJson => Option[Product] ): IndexedSeq[Product] = {
      productsByState.getOrElse(state, Vector()).flatMap { p => f(p) } // remove the Option in Option[Product]
    }

    if (requiredSize <= 0) return accumItems
    // specify the URI for the LCBO api url for liquor selection
    val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo) // get as many as possible on a page because we could have few matches.
    logger.info(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = (for (p <- itemNodes) yield p.extract[ProductAsLCBOJson].removeNulls).filter(myFilter)  // LCBO sends us poisoned useless nulls that we need to filter for DB (filter them right away).
    val outstandingSize = requiredSize - items.size

    // Collects into our list of products the attributes we care about (extract[Product]). Then filter out unwanted data.
    // fyi: throws Mapping exception.
    //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)

    // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
    val productsByState: Map[EntityRecordState, IndexedSeq[ProductAsLCBOJson]] = items.groupBy {
      p => (dbProducts.get(p.id), p) match {
        case (None, _)  => New
        case (Some(product), lcboProduct) if product.isDirty(lcboProduct) => Dirty
        case (_ , _) => Clean
      }
    }

    val cleanProducts = accumItems(Clean) ++ fetchItems(Clean, productsByState, {p => p.getProduct(dbProducts)})
    val dirtyProducts = accumItems(Dirty) ++ fetchItems(Dirty, productsByState, {p => p.getProduct(dbProducts).map({ _.copyAttributes( p)})})
    val newProducts = accumItems(New) ++ productsByState.getOrElse(New, Nil).map{ p => Product.create(p) }
    val revisedAccumItems: Map[EntityRecordState, IndexedSeq[Product]] = Map(New -> newProducts, Dirty -> dirtyProducts, Clean -> cleanProducts)

    if (outstandingSize <= 0 || isFinalPage || totalPages < nextPage) return revisedAccumItems
    // Deem as last page only if  LCBO tells us it's final page or we evaluate next page won't have any (when we gap due to parallelism).
    // Similarly having reached our required size,we can stop.

    // tail recursion enforced.
    collectItemsOnAPage(
      dbProducts,
      revisedAccumItems,
      urlRoot,
      outstandingSize,
      nextPage,
      pageDelta,
      myFilter) // union of this page with next page when we are asked for a full sample
  }
}