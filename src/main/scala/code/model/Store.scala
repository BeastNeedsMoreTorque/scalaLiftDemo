package code.model

import scala.collection.concurrent
import scala.language.implicitConversions
import scala.xml.Node
import scala.collection.concurrent.TrieMap


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
                                 city: String = "") {}

class Store private() extends Record[Store] with KeyedRecord[Long] with CreatedUpdated[Store]  {
  def meta = Store

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  lazy val userStores = MainSchema.storeToUserStores.left(this)

  val lcbo_id = new IntField(this) // we don't share same PK as LCBO!
  val is_dead = new BooleanField(this, false)
  val distance_in_meters = new IntField(this)
  val latitude = new DoubleField(this)
  val longitude = new DoubleField(this)

  val name = new StringField(this, 200)
  val address_line_1 = new StringField(this, 400)
  val city = new StringField(this, 80)

  def synchUp(s: PlainStoreAsLCBOJson): Unit = {
    def isDirty(s: PlainStoreAsLCBOJson): Boolean = {
      is_dead.get != s.is_dead ||
        address_line_1.get != s.address_line_1
    }
    def copyAttributes(p: PlainStoreAsLCBOJson): Unit = {
      is_dead.set(p.is_dead)
      address_line_1.set(p.address_line_1)
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
  def MaxSampleSize = Props.getInt("store.maxSampleSize", 0)
  private val storesCache: concurrent.Map[Int, Store] = TrieMap[Int, Store]()


  def init() = { // could help queries find(lcbo_id)
    def loadAll(dbStores: Map[Int, Store]): Box[Map[Int, Store]] = {
      val branchCountUpperBound = Props.getInt("store.BranchCountUpperBound", 0)
      // we'd like the is_dead ones as well to update state (but apparently you have to query for it explicitly!?!?)
      val url = s"$LcboDomainURL/stores?"
      tryo { collectStoresOnAPage(dbStores, Map[Int, Store](), url, branchCountUpperBound, pageNo = 1) }
    }

    def getStores(): Map[Int, Store] = {
      val synchLcbo = Props.getBool("store.synchLcbo", true)
      inTransaction {
        val dbStores = stores.map(s => s.lcbo_id.get -> s)(collection.breakOut): Map[Int, Store] // queries full store table and throw it into map
        if (synchLcbo) loadAll(dbStores) match {
          case Full(m) => m // returns map normally // going to LCBO and update DB afterwards with fresh data when we see breaks with LCBO data
          case Failure(m, ex, _) => logger.error(s"Problem loading LCBO stores into cache with message $m and exception error $ex")
            Map[Int, Store]()
          case Empty => logger.error("Problem loading LCBO stores into cache, none found")
            Map[Int, Store]()
        }
        else dbStores // configuration tells us to trust our db contents
      }
    }
    storesCache ++= getStores()
  }

  def create(s: StoreAsLCBOJson): Store = {
    // store in same format as received by provider so that un-serializing if required will be same logic. This boiler-plate code seems crazy (not DRY at all)...
    createRecord.
      lcbo_id(s.id).
      name(s.name).
      is_dead(s.is_dead).
      distance_in_meters(s.distance_in_meters).
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
    logger.debug(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children.drop(1) // Uses XPath-like querying to extract data from parsed object jsObj.
    itemNodes.map(_.extract[StoreAsLCBOJson])
  }




  private final def getSingleStore( uri: String): PlainStoreAsLCBOJson = {
    logger.debug(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    (parse(pageContent) \ "result").extract[PlainStoreAsLCBOJson] // more throws
  }

 // collects stores individually from LCBO REST as PlainStoreAsLCBOJson on as many pages as required and convert
  // temporarily lists into Maps we can use indexing by LCBO ID. Along the way, we compare each such LCBO record
  // with its equivalent content in database (call to fetchSynched) to update our database content if it's out of date.
  @scala.annotation.tailrec
  private final def collectStoresOnAPage( dbStores: Map[Int, Store],
                                         accumItems: Map[Int, Store],
                                         urlRoot: String,
                                         requiredSize: Int,
                                         pageNo: Int): Map[Int, Store] = {
    val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo)
    logger.info(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children // Uses XPath-like querying to extract data from parsed object jsObj.
    // get all the stores from JSON itemNodes, extract them and map them to usable Store class after synching it with our view of same record in database.
    val pageStoreMap = {for (p <- itemNodes) yield p.extract[PlainStoreAsLCBOJson]}.map(st => st.id -> fetchSynched(st, dbStores.get(st.id)))
    val outstandingSize = requiredSize - pageStoreMap.size
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extract[Boolean]
    if (pageStoreMap.isEmpty || outstandingSize <= 0 || isFinalPage) return accumItems ++ pageStoreMap  // no need to look at more pages

    collectStoresOnAPage(
      dbStores,
      accumItems ++ pageStoreMap,
      urlRoot,
      outstandingSize,
      pageNo + 1) // union of this page with next page when we are asked for a full sample
  }

  def fetchSynched(s: PlainStoreAsLCBOJson, ourCachedStore: Box[Store]): Store = {
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
      ourCachedStore.map { t =>
        t.synchUp(s) // touch it up if dirty
        t
      } openOr { // we don't have it in our DB, so insert it.
        val t = create(s)
        t.save
        t
      }
    }
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