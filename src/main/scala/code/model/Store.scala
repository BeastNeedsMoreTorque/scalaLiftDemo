package code.model

import java.io.IOException

import code.model.Product._

import scala.annotation.tailrec
import scala.collection.{Iterable, Set, Map, concurrent}
import scala.language.implicitConversions
import scala.util.Random
import scala.xml.Node
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import net.liftweb.db.DB
import net.liftweb.common.{Full, Empty, Box, Failure, Loggable}
import net.liftweb.json._
import net.liftweb.json.JsonParser.{ParseException, parse}
import net.liftweb.util.Helpers.tryo
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.record.field._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.{DefaultConnectionIdentifier, Props}

import org.squeryl.annotations._

import code.Rest.pagerRestClient
import MainSchema._
import code.snippet.SessionCache.theStoreId

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

  // intentional change of scale from metres to kilometres, using String representation instead of integer and keeping 3 decimals (0.335 ml for beer)
  def distanceInKMs: String = {
    val d = distance_in_meters / 1000.0
    f"$d%1.1f KM(s)"
  }

  override def toString = s"$id, name: $name, Address: $address_line_1, city: $city, distance is:$distanceInKMs"

  /**
    *
    * @return an ordered list of pairs of values (label and value), representing most of the interesting data of the product
    */
  def createProductElemVals: List[(String, String)] =
    (("Name: ", name) ::
      ("Primary Address: ", address_line_1) ::
      ("City: ", city) ::
      ("Your Distance: ", distanceInKMs) ::
      ("Latitude: ", latitude.toString) ::
      ("Longitude: ", longitude.toString) ::
      Nil).filter({ p: (String, String) => p._2 != "null" && !p._2.isEmpty })

}

object StoreAsLCBOJson {
  def apply(s: Store) = new StoreAsLCBOJson(s)

  private implicit val formats = net.liftweb.json.DefaultFormats

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
  def meta = Store

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  lazy val userStores = MainSchema.storeToUserStores.left(this)

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

  def isDirty(s: PlainStoreAsLCBOJson): Boolean = {
    is_dead.get != s.is_dead ||
    address_line_1.get != s.address_line_1
  }

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
}

object Store extends Store with MetaRecord[Store] with pagerRestClient with Loggable {
  private implicit val formats = net.liftweb.json.DefaultFormats
  override def MaxPerPage = Props.getInt("store.lcboMaxPerPage", 0)
  override def MinPerPage = Props.getInt("store.lcboMinPerPage", 0)
  private val PositiveInventoryIterations = Props.getInt("store.PositiveInventoryIterations",  10)

  private val MaxSampleSize = Props.getInt("store.maxSampleSize", 0)
  private val DBBatchSize = Props.getInt("store.DBBatchSize", 1)
  private val storeLoadWorkers = Props.getInt("store.load.workers", 1)
  private val synchLcbo = Props.getBool("store.synchLcbo", true)

  private val storeCategoriesProductsCache: concurrent.Map[(Int, String), Set[Int]] = TrieMap() // give set of available productIds by store+category
  private val storeProductsLoaded: concurrent.Map[Int, Unit] = TrieMap() // effectively a thread-safe lock-free set, which helps control access to storeCategoriesProductsCache.

  // would play a role if user selects a store from a map. A bit of an exercise for caching and threading for now.
  private val storesCache: concurrent.Map[Int, Store] = TrieMap[Int, Store]()

  def fetchItems(state: EntityRecordState,
                 mapList: Map[EntityRecordState, List[PlainStoreAsLCBOJson]],
                 f: PlainStoreAsLCBOJson => Option[Store] ): List[Store] =
    mapList.getOrElse(state, Nil).flatMap{ f(_)}

  def init() = { // could help queries find(lcbo_id)
    def synchronizeData(idx: Int, dbStores: Map[Int, Store]): Box[Map[Int, Store]] = {
      // we'd like the is_dead ones as well to update state (but apparently you have to query for it explicitly!?!?)
      val url = s"$LcboDomainURL/stores?"
      tryo {
        // gather stores on this page (url, idx) with classification as to whether they are new, dirty or clean
        val initMap = Map[EntityRecordState, List[Store]](New -> Nil, Dirty -> Nil, Clean -> Nil)
        val lcboStoresPerWorker = collectStoresOnAPage(dbStores, initMap, url, idx)
        logger.trace(s"done loading to LCBO")

        // 0.9 sec to do this work with 1 thread (going to DB one by one). With 2 threads and batch access to DB went down to 0.250 sec
        // on about 400 records to update and 650 records total. When no change is required to DB, this would take 0.200 sec. (MacBook Air 2010)
        // With more complex solution, whole thing takes 2.7 secs and about 170 ms on db side end processing (best case).
        // identify the dirty and new stores for batch update and then retain all of them as the set identified by the worker (not visibly slower than when there is no change to DB)
        inTransaction {
          // for the activity on separate thread for synchronizeData
          // batch update the database now.
          updateStores(lcboStoresPerWorker(Dirty))
          insertNewStores(lcboStoresPerWorker(New))
        }
        lcboStoresPerWorker.values.flatten.map(s => s.lcbo_id.get -> s).toMap // flatten the 3 lists and then build a map from the stores keyed by lcbo_id.
      }
    }

    def getStores(idx: Int, dbStores: Map[Int, Store]): Map[Int, Store] = {
      if (synchLcbo) synchronizeData(idx, dbStores) match {
          case Full(m) => m
          case Failure(m, ex, _) => throw new Exception(s"Problem loading LCBO stores into cache (worker $idx) with message '$m' and exception error '$ex'")
          case Empty =>  throw new Exception(s"Problem loading LCBO stores into cache (worker $idx), none found")
      }
      else dbStores // configuration tells us to trust our db contents
    }

    logger.trace(s"Store.init")

    inTransaction {  // for the initial select
      import scala.util.{Success, Failure}
      val dbStores = stores.map(s => s.lcbo_id.get -> s)(collection.breakOut): Map[Int, Store] // queries full store table and throw it into map
      for (i <- 1 to storeLoadWorkers) {
        val fut = Future { getStores(i, dbStores) } // do this asynchronously to be responsive asap (default 1 worker).
        fut onComplete {
          case Success(m) => storesCache ++= m
            logger.trace(s"Store.init worker completed with success")
          case Failure(t) => logger.error(s"Store.init ${t.getMessage} " )
          // don't attempt to reset storesCache here as we crossed the bridge that we want to trust the current LCBO data.
          // We could define a finer policy as to when we want to use a default set from database even when we attempt to go to LCBO.
        }
      }
    }
  }


  /**
    * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
    * Select a random product that matches the parameters subject to a max sample size.
    *
    * @param store a String representing a numeric code of a LCBO store
    * @param category a String such as beer, wine, mostly matching primary_category at LCBO, or an asset category.
    * @return
    */
  def recommend(storeId: Int, category: String): Box[(Product, Int)] = {
    @tailrec // iterate in sampling until we get a non-zero inventory at the store or we give up after a few trials
    def sampleWithInventory(trial: Int, prodIds: Set[Int]): Option[(Product, Int)] = {
      val randomIndex = Random.nextInt(math.max(1, prodIds.size))
      val prodId = prodIds.takeRight(randomIndex).head // by virtue of test, there should be some (assumes we never remove from cache to reduce set, otherwise we'd need better locking here).
      val quantity = StoreProduct.getStoreProduct(storeId, prodId).fold(0){ _.quantity.get} // they might actually be often out of stock, or it could be dead
      if (quantity > 0 || trial > PositiveInventoryIterations )
        for ( p <- Product.getProduct(prodId)) yield (p, quantity)
      else
        sampleWithInventory(trial+1, prodIds)
    }

    // note: cacheSuccess grabs a lock
    def cacheSuccess(storeId: Int, category: String): Option[(Product, Int)] = {
      if (storeCategoriesProductsCache.contains((storeId, category)) && !storeCategoriesProductsCache((storeId, category)).isEmpty) {
        val prodIds = storeCategoriesProductsCache((storeId, category))
        sampleWithInventory(0, prodIds)
      }
      else Empty
    }

    tryo {
      cacheSuccess(storeId, LiquorCategory.toPrimaryCategory(category)).map { identity} getOrElse {
        loadCache(storeId) // get the full store inventory in background for future requests and then respond to immediate request.
        val randomIndex = Random.nextInt(math.max(1, MaxSampleSize)) // max constraint is defensive for poor client usage (negative numbers).
        val prods = productListByStoreCategory(randomIndex + 1, storeId, category) // index is 0-based but requiredSize is 1-based so add 1,
        val randKey = prods.keySet.takeRight(randomIndex+1).head
        val inv = StoreProduct.inventoryForStoreProduct(storeId, randKey)
        (prods(randKey), inv.quantity)
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

  def findInRectangle( lat1: String, lon1: String,
                 lat2: String, lon2: String): Box[Iterable[StoreAsLCBOJson]] =  {
    def inRectangle(s: StoreAsLCBOJson): Boolean = {
      val lat = s.latitude
      val lon = s.longitude

      lat >= lat1.toDouble && lat <= lat2.toDouble &&
      lon >= lon1.toDouble && lon <= lon2.toDouble
    }
    Full(storesCache.values.map(s => StoreAsLCBOJson(s)).filter(inRectangle))
  }

  /**
    * Find the closest store by coordinates, caching is not applicable as we cannot guess what store is closest to input
    * unless we do a geo query in DB ourselves, which is excessive effort given LCBO API.
    */
  def find( lat: String,  lon: String): Box[StoreAsLCBOJson] =  {
    def findStore(lat: String, lon: String): Box[StoreAsLCBOJson] = {
      val url = s"$LcboDomainURL/stores?where_not=is_dead" +
        additionalParam("lat", lat) +
        additionalParam("lon", lon)
      tryo {
        val b: Box[StoreAsLCBOJson] = collectFirstMatchingStore(url).headOption
        b.openOrThrowException(s"No store found near ($lat, $lon)") // it'd be a rare event not to find a store here. Exception will be caught immediately by tryo.
      }
    }

    findStore(lat, lon) match {
      case Full(x) =>
        theStoreId.set(x.id)
        Full(x)
      case Failure(msg, exc, _) =>
        logger.error(s"unable to find closest store with error $msg exception $exc")
        Empty
      case Empty =>
        logger.error("unable to find closest store info")
        Empty
    }
  }

  @throws(classOf[net.liftweb.json.MappingException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  private final def collectFirstMatchingStore( uri: String): List[StoreAsLCBOJson] = {
    logger.trace(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children.drop(1) // Uses XPath-like querying to extract data from parsed object jsObj.
    itemNodes.map(_.extract[StoreAsLCBOJson])
  }

  @tailrec
  def updateStores(myStores: Iterable[Store]): Unit = {
    val slice = myStores.take(DBBatchSize)
    stores.update(slice)
    val rest = myStores.takeRight(myStores.size - slice.size)
    if (!rest.isEmpty) updateStores( rest)
  }

  @tailrec
  def insertNewStores( myStores: Iterable[Store]): Unit = {
    val slice = myStores.take(DBBatchSize)
    stores.insert(slice)
    val rest = myStores.takeRight(myStores.size - slice.size)
    if (!rest.isEmpty) insertNewStores(rest)
  }

  private final def getSingleStore( uri: String): PlainStoreAsLCBOJson = {
    logger.debug(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    (parse(pageContent) \ "result").extract[PlainStoreAsLCBOJson] // more throws
  }


  // collects stores individually from LCBO REST as PlainStoreAsLCBOJson on as many pages as required.
  // we declare types fairly often in the following because it's not trivial to follow otherwise
  @scala.annotation.tailrec
  private final def collectStoresOnAPage(dbStores: Map[Int, Store],
                                         accumItems: Map[EntityRecordState, List[Store]],
                                         urlRoot: String,
                                         pageNo: Int): Map[EntityRecordState, List[Store]] = {
    val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo)
    logger.info(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children // Uses XPath-like querying to extract data from parsed object jsObj.
    // get all the stores from JSON itemNodes, extract them and map them to usable Store class after synching it with our view of same record in database.
    val pageStoreSeq = {for (p <- itemNodes) yield p.extract[PlainStoreAsLCBOJson]}

    // partition pageStoreSeq into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
    val storesByState: Map[EntityRecordState, List[PlainStoreAsLCBOJson]] = pageStoreSeq.groupBy {
      s => (dbStores.get(s.id), s) match {
        case (None, _)  => New
        case (Some(store), lcboStore) if store.isDirty(lcboStore) => Dirty
        case (_ , _) => Clean
      }
    }

    val cleanStores = accumItems(Clean) ++ fetchItems(Clean, storesByState, s => s.getStore(dbStores))
    val dirtyStores = accumItems(Dirty) ++ fetchItems(Dirty, storesByState, s => s.getStore(dbStores).map(copyAttributes(_, s) ))
    val newStores = accumItems(New) ++ (storesByState.getOrElse(New, Nil)).map{ create }

    // after preliminaries, get the map of stores indexed properly by state that we need having accumulated over the pages so far.
    val revisedAccumItems = Map(New -> newStores, Dirty -> dirtyStores, Clean -> cleanStores)

    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extract[Boolean]

    if (pageStoreSeq.isEmpty || isFinalPage) return revisedAccumItems  // no need to look at more pages

    collectStoresOnAPage(
      dbStores,
      revisedAccumItems,
      urlRoot,
      pageNo + storeLoadWorkers) // union of this page with next page when we are asked for a full sample
  }

  def find( lcbo_id: Int): Box[Store] =  {
    if (storesCache.contains(lcbo_id)) Full(storesCache(lcbo_id))
    else findStore(lcbo_id) match {
      case Full(x) =>
        theStoreId.set(x.lcbo_id.get)
        Full(x)
      case Failure(msg, exc, _) =>
        logger.error(s"unable to find closest store with error $msg exception $exc")
        Empty
      case Empty =>
        logger.error("unable to find closest store info")
        Empty
    }
  }

  private def findStore(lcbo_id: Int): Box[Store] = {
    val url = s"$LcboDomainURL/stores/$lcbo_id"
    logger.debug(s"findStore by id using $url")
    tryo { fetchSynched(getSingleStore(url))}
  }

  def fetchSynched(s: PlainStoreAsLCBOJson): Store = {
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

    DB.use(DefaultConnectionIdentifier) { connection =>
      val o: Box[Store] = stores.where( _.lcbo_id === s.id).headOption // Load from recent DB cache if available, else create it Squeryl very friendly DSL syntax!
      o.map { t: Store =>
        t.synchUp(s) // touch it up with most recent data if dirty
        t
      } openOr {
        val t = create(s)
        t.save
        UserStore.createRecord.userid(User.id.get).storeid(t.id).save // cascade save dependency.
        t
      }
    }
  }

  import java.net.SocketTimeoutException

  // for reflection and generating documentation
  /**
    * Queries LCBO matching category and storeId for a sample size as specified by client, with category considered optional, though not tested when optional.
    * Full URL will be built as follows: http://lcbo.com/products?store_id=<storeId>&q=<category.toLowerCase()>&per_page=<perPage>
    * LCBO allows to specify q as query to specify pattern match on product name (e.g. beer, wine)
    * for pattern match LCBO uses lower case but for actual product category it's upper case, so to make comparisons, we will need to account for that
    * primary_category in catalog or p.primary_category so we need a conversion function to adjust)
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
  private def productListByStoreCategory(requiredSize: Int, store: Long, category: String): Map[Int, Product] = {
    if (requiredSize <= 0) Map()
    else {
      val url = s"$LcboDomainURL/products?store_id=$store" + additionalParam("q", category) // does not handle first one such as storeId, which is artificially mandatory
      val filter = { p: ProductAsLCBOJson => p.primary_category == LiquorCategory.toPrimaryCategory(category) &&
        !p.is_discontinued
      } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers
      val initMap = Map[EntityRecordState, List[Product]](New -> Nil, Clean -> Nil, Dirty -> Nil)
      val items = collectItemsOnAPage(
        Map[Int, Product](),  // suppress reconciliation with database, just trust LCBO now.
        initMap,
        url,
        requiredSize,
        pageNo = 1,
        pageDelta = 1,
        filter)
      val productInventories: Iterable[Product] = items.values.take(requiredSize).flatten // we don't care about Clean/New/Dirty state here so flatten values.
      productInventories.map(p => p.lcbo_id.get -> p).toMap  // build a map for each product using lcbo_id as key.
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
  private def productListByStore(dbProducts: Map[Int, Product],
                                 requiredSize: Int,
                                 store: Long,
                                 page: Int): Map[EntityRecordState, List[Product]] = {
    if (requiredSize <= 0) Map()
    else {
      val url = s"$LcboDomainURL/products?store_id=$store"
      val filter = { p: ProductAsLCBOJson => !p.is_discontinued } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers
      val initMap = Map[EntityRecordState, List[Product]] (New -> Nil, Dirty -> Nil, Clean -> Nil)

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
  private def storeProductListByStore(dbStoreProducts: Map[Int, StoreProduct],
                                 requiredSize: Int,
                                 store: Int,
                                 page: Int): Map[EntityRecordState, List[StoreProduct]] = {
    if (requiredSize <= 0) Map()
    else {
      val url = s"$LcboDomainURL/inventories?store_id=$store"
      val filter = { p: InventoryAsLCBOJson => !p.is_dead } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers
      val initMap = Map[EntityRecordState, List[StoreProduct]] (New -> Nil, Dirty -> Nil, Clean -> Nil)

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
                                        accumItems: Map[EntityRecordState, List[Product]],
                                        urlRoot: String,
                                        requiredSize: Int,
                                        pageNo: Int,
                                        pageDelta: Int = 1,
                                        myFilter: ProductAsLCBOJson => Boolean = { p: ProductAsLCBOJson => true }): Map[EntityRecordState, List[Product]] = {
    def nextPage() = pageNo + prodLoadWorkers
    def fetchItems(state: EntityRecordState,
                   mapList: Map[EntityRecordState, List[ProductAsLCBOJson]],
                   f: ProductAsLCBOJson => Option[Product] ): List[Product] = {
      mapList.getOrElse(state, Nil).flatMap { p => f(p) } // remove the Option in Option[Product]
    }

    if (requiredSize <= 0) accumItems
    // specify the URI for the LCBO api url for liquor selection
    val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo) // get as many as possible on a page because we could have few matches.
    logger.info(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = (for (p <- itemNodes) yield p.extract[ProductAsLCBOJson].removeNulls).filter(myFilter)  // LCBO sends us poisoned useless nulls that we need to filter for DB (filter them right away).
    val outstandingSize = requiredSize - items.size

    // Collects into our list of products the attributes we care about (extract[Product]). Then filter out unwanted data.
    // fyi: throws Mapping exception.
    //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extract[Boolean]
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extract[Int]

    // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
    val productsByState: Map[EntityRecordState, List[ProductAsLCBOJson]] = items.groupBy {
      p => (dbProducts.get(p.id), p) match {
        case (None, _)  => New
        case (Some(product), lcboProduct) if product.isDirty(lcboProduct) => Dirty
        case (_ , _) => Clean
      }
    }

    val cleanProducts = accumItems(Clean) ++ fetchItems(Clean, productsByState, {p => p.getProduct(dbProducts)})
    val dirtyProducts = accumItems(Dirty) ++ fetchItems(Dirty, productsByState, {p => p.getProduct(dbProducts).map({ _.copyAttributes( p)})})
    val newProducts = accumItems(New) ++ productsByState.getOrElse(New, Nil).map{ p => Product.create(p) }
    val revisedAccumItems = Map(New -> newProducts, Dirty -> dirtyProducts, Clean -> cleanProducts)

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

  @throws(classOf[net.liftweb.json.MappingException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @tailrec
  private final def collectStoreProductsOnAPage(dbStoreProducts: Map[Int, StoreProduct],
                                        accumItems: Map[EntityRecordState, List[StoreProduct]],
                                        urlRoot: String,
                                        requiredSize: Int,
                                        pageNo: Int,
                                        pageDelta: Int = 1,
                                        myFilter: InventoryAsLCBOJson => Boolean = { sp: InventoryAsLCBOJson => true }): Map[EntityRecordState, List[StoreProduct]] = {
    def nextPage() = pageNo + prodLoadWorkers
    def fetchItems(state: EntityRecordState,
                   mapList: Map[EntityRecordState, List[InventoryAsLCBOJson]],
                   f: InventoryAsLCBOJson => Option[StoreProduct] ): List[StoreProduct] = {
      mapList.getOrElse(state, Nil).flatMap { p => f(p) } // remove the Option in Option[Product]
    }

    if (requiredSize <= 0) accumItems
    // specify the URI for the LCBO api url for liquor selection
    val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo) // get as many as possible on a page because we could have few matches.
    logger.info(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = (for (p <- itemNodes) yield p.extract[InventoryAsLCBOJson].removeNulls).filter(myFilter)  // LCBO sends us poisoned useless nulls that we need to filter for DB (filter them right away).
    val outstandingSize = requiredSize - items.size

    // Collects into our list of products the attributes we care about (extract[Product]). Then filter out unwanted data.
    // fyi: throws Mapping exception.
    //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extract[Boolean]
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extract[Int]

    // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
    val storeProductsByState: Map[EntityRecordState, List[InventoryAsLCBOJson]] = items.groupBy {
      i => (dbStoreProducts.get(i.product_id), i) match {
        case (None, _)  => New
        case (Some(storeProduct), lcboInventory) if storeProduct.isDirty(lcboInventory) => Dirty
        case (_ , _) => Clean
      }
    }

    val cleanProducts = accumItems(Clean) ++ fetchItems(Clean, storeProductsByState, {inv => inv.getProduct(dbStoreProducts)})
    val dirtyProducts = accumItems(Dirty) ++ fetchItems(Dirty, storeProductsByState, {inv => inv.getProduct(dbStoreProducts).map({ _.copyAttributes( inv)})})
    val newProducts = accumItems(New) ++ storeProductsByState.getOrElse(New, Nil).map{ sp => StoreProduct.create(sp) }
    val revisedAccumItems = Map(New -> newProducts, Dirty -> dirtyProducts, Clean -> cleanProducts)

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

  def loadAll(dbProducts: Map[Int, Product], requestSize: Int, storeId: Int, page: Int): Box[Map[EntityRecordState, List[Product]]] =
    tryo { productListByStore(dbProducts, requestSize, storeId, page) }

  def loadAllStoreProducts(dbStoreProducts: Map[Int, StoreProduct], requestSize: Int, storeId: Int, page: Int): Box[Map[EntityRecordState, List[StoreProduct]]] =
    tryo { storeProductListByStore(dbStoreProducts, requestSize, storeId, page) }


  // may have side effect to update database with more up to date from LCBO's content (if different)
  // lock free. For correctness, we use putIfAbsent to get some atomicity when we do complex read-write at once (complex linearizable operations).
  // For now, we call this lazily for first user having an interaction with a particular store and not before.
  def loadCache(storeId: Int) = {

    // Uses convenient Squeryl native Scala ORM for a simple 2-table join.
    def GetStoreProductsFromDB: Map[Int, (Product, StoreProduct)] =
      inTransaction {
        val prods = from(storeProducts, products)((sp, p) =>
          where(sp.storeid === storeId and sp.productid === p.lcbo_id  )
            select (p, sp) )
        prods.map(pair => pair._1.lcbo_id.get -> pair).toMap
      }


    def getProductsOfStartPage(page: Int, dbProducts: Map[Int, Product], allDbProductIds: Set[Int]): Map[Int, Product] =
      inTransaction {
        if (synchLcbo) {
          loadAll(dbProducts, productCacheSize, storeId, page) match {
            case Full(m) =>  // Used to be 10 seconds to get here after last URL query, down to 0.046 sec
              for ((k, v) <- m) { // v List[Product] containing product
                k match {
                  case New =>
                    insertNewProducts(v.filter({p => !allDbProductIds.contains(p.lcbo_id.get)} )) // insert to DB those we didn't get in our query to obtain allDbProducts
                  case Dirty =>
                    updateProducts(v)  // discard inventory that we don't need. This is just conveniently realizing our product is out of date and needs touched up to DB.
                  case _ => ; // no-op if clean
                }
              }
              // 5 seconds per thread (pre Jan 23 with individual db writes). Now 1.5 secs for a total of 2-3 secs, compared to at least 15 secs.
              m.values.flatten.map(p => p.lcbo_id.get -> p).toMap // flatten the 3 lists and then build a map from the stores keyed by lcbo_id.
            case Failure(m, ex, _) =>
              logger.error(s"Problem loading products into cache with message $m and exception error $ex")
              Map[Int, Product]()
            case Empty =>
              logger.error("Problem loading products into cache")
              Map[Int, Product]()
          }
        }
        else if (page == 1) dbProducts // just load from DB without going to LCBO. And if config is weird to use multiple threads, do it only on first one.
        else Map[Int, Product]()
      }

    def getStoreProductsOfStartPage(page: Int, dbStoreProducts: Map[Int, StoreProduct], allDbProductIds: Set[Int]): Map[Int, StoreProduct] =
      inTransaction {
        if (synchLcbo) {
          loadAllStoreProducts(dbStoreProducts, productCacheSize, storeId, page) match {
            case Full(m) =>  // Used to be 10 seconds to get here after last URL query, down to 0.046 sec
              for ((k, v) <- m) { // v List[StoreProduct] containing product
                k match {
                  case New =>
                    StoreProduct.insertStoreProducts(v) // now we can insert relationship of store-product with an inventory of 10
                  case Dirty =>
                    StoreProduct.updateStoreProducts(v)
                  case _ => ; // no-op if clean
                }
              }
              // 5 seconds per thread (pre Jan 23 with individual db writes). Now 1.5 secs for a total of 2-3 secs, compared to at least 15 secs.
              m.values.flatten.map(sp => sp.productid.get -> sp).toMap // flatten the 3 lists and then build a map from the stores keyed by lcbo_id.
            case Failure(m, ex, _) =>
              logger.error(s"Problem loading products into cache with message $m and exception error $ex")
              Map[Int, StoreProduct]()
            case Empty =>
              logger.error("Problem loading products into cache")
              Map[Int, StoreProduct]()
          }
        }
        else if (page == 1) dbStoreProducts // just load from DB without going to LCBO. And if config is weird to use multiple threads, do it only on first one.
        else Map[Int, StoreProduct]()
      }

    val dbProductsAndStoreProducts: Map[Int, (Product, StoreProduct)] = GetStoreProductsFromDB
    val dbProducts = for ((k, v) <- dbProductsAndStoreProducts) yield (k, v._1)
    val dbStoreProducts = for ((k, v) <- dbProductsAndStoreProducts) yield (k, v._2)

    val allDbProductIds: Set[Int] = inTransaction { products.map(_.lcbo_id.get).toSet } // this "products" is actually a query.

    def fetchInventories(initialPages: List[Int], dbStoreProducts: Map[Int, StoreProduct], allDbProductIds: Set[Int]) = {
      val allTheStoreInventories = Future.traverse(initialPages)(storeProductsByWorker)

      allTheStoreInventories onSuccess {
        case maps: Iterable[Map[Int, StoreProduct]] =>
          val fullMap: Map[Int, StoreProduct] = maps.flatten.toMap
          StoreProduct.update(storeId, fullMap)
      }

      allTheStoreInventories onFailure {
        case t => logger.error("loadCache, an error for inventories occurred because: " + t.getMessage)
      }
    }

    // evaluate the maps of product in parallel, tracking them by product id (lcbo_id) as key but in order to be able to cache them by category when aggregating results back.
    def productsByWorker(page: Int): Future[ Map[Int, Product]] =
      Future { getProductsOfStartPage(page, dbProducts, allDbProductIds) }

    def storeProductsByWorker(page: Int): Future[ Map[Int, StoreProduct]] =
      Future { getStoreProductsOfStartPage(page, dbStoreProducts, allDbProductIds) }


    logger.trace(s"loadCache start $storeId") // 30 seconds from last LCBO query to completing the cache update (Jan 23). Needs to be better.
    if ( storeProductsLoaded.putIfAbsent(storeId, Unit).isEmpty) {
      // A kind of guard: Two piggy-backed requests to loadCache for same store will thus ignore second one.
      // Slightly unwanted consequence is that clients need to test for empty set and not assume it's non empty.
      val initialParallelPages = (1 to prodLoadWorkers).toList  // e.g. (1,2,3,4) when using 4 threads, used to aggregate the results of each thread.
      val allTheProductsByCategory = Future.traverse(initialParallelPages)(productsByWorker)

      // we assume all along and depend strongly on that, that we never remove from cache. Updates that are strictly monotonic are easier to get right.
      allTheProductsByCategory onSuccess {
        case maps: Iterable[Map[Int, Product]] =>
          val fullMap = maps.flatten
          for ((id, p) <- fullMap) {
            val storeCatKey = (storeId, p.primary_category.get)
            if (!storeCategoriesProductsCache.putIfAbsent(storeCatKey, Set(id)).isEmpty  ) {
              // if was empty, we just initialized it atomically with set(id) but if there was something, just add to set below
              storeCategoriesProductsCache(storeCatKey) +=  id
            }
          }
          Product.update(fullMap.toMap)
          logger.trace(s"product (store,categories) keys ${storeCategoriesProductsCache.keys}")
          fetchInventories(initialParallelPages, dbStoreProducts, allDbProductIds)

          }

      allTheProductsByCategory onFailure {
        case t => logger.error("loadCache, an error occurred because: " + t.getMessage)
      }
    }
  }
}