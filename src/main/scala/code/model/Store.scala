package code.model

import scala.annotation.tailrec
import scala.collection.{Map,concurrent}
import scala.language.implicitConversions
import scala.xml.Node
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import net.liftweb.db.DB
import net.liftweb.common.{Full, Empty, Box, Failure, Loggable}
import net.liftweb.json._
import net.liftweb.json.JsonParser.parse
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
  private implicit val formats = net.liftweb.json.DefaultFormats

  /**
    * Convert a store to XML
    */
  implicit def toXml(st: StoreAsLCBOJson): Node =
    <item>{Xml.toXml(st)}</item>


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
  private val MaxSampleSize = Props.getInt("store.maxSampleSize", 0)
  private val DBBatchSize = Props.getInt("store.DBBatchSize", 1)
  private val storeLoadWorkers = Props.getInt("store.load.workers", 1)
  private val synchLcbo = Props.getBool("store.synchLcbo", true)


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

}