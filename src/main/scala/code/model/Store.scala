package code.model

import scala.language.implicitConversions
import scala.xml.Node

import net.liftweb.common.{Full, Empty, Box,Failure, Loggable}
import net.liftweb.db.DB
import net.liftweb.json._
import net.liftweb.json.JsonParser.parse
import net.liftweb.util.Helpers.tryo
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.record.field._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.util.{DefaultConnectionIdentifier, Props}
import net.liftweb.squerylrecord.RecordTypeMode._

import org.squeryl.annotations._

import code.Rest.pagerRestClient
import code.snippet.SessionCache.theStoreId
import MainSchema._

/**
  * Created by philippederome on 15-11-01.
  * This is captured from JSON parsing.
  */
case class StoreAsLCBOJson(id: Int = 0,
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

class Store private() extends Record[Store] with KeyedRecord[Long] with CreatedUpdated[Store]  {
  def meta = Store

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  val lcbo_id = new IntField(this) // we don't share same PK as LCBO!
  val is_dead = new BooleanField(this, false)
  val distance_in_meters = new IntField(this)
  val latitude = new DoubleField(this)
  val longitude = new DoubleField(this)

  val name = new StringField(this, 200)
  val address_line_1 = new StringField(this, 400)
  val city = new StringField(this, 80)



  def synchUp(s: StoreAsLCBOJson): Unit = {
    def isDirty(s: StoreAsLCBOJson): Boolean = {
      is_dead.get != s.is_dead ||
      address_line_1.get != s.address_line_1
    }
    def copyAttributes(p: StoreAsLCBOJson): Unit = {
      is_dead.set(p.is_dead)
      address_line_1.set(p.address_line_1)
    }
    if (isDirty(s)) {
      copyAttributes(s)
      this.update  // Active Record pattern
    }
  }

}

object Store extends Store with MetaRecord[Store] with pagerRestClient with Loggable {
  private implicit val formats = net.liftweb.json.DefaultFormats
  override def MaxPerPage = Props.getInt("store.lcboMaxPerPage", 0)
  override def MinPerPage = Props.getInt("store.lcboMinPerPage", 0)
  def MaxSampleSize = Props.getInt("store.maxSampleSize", 0)

  def fetchSynched(s: StoreAsLCBOJson) = {
    DB.use(DefaultConnectionIdentifier) { connection =>
      val o: Box[Store] = stores.where(_.lcbo_id === s.id).forUpdate.headOption // Load from DB if available, else create it Squeryl very friendly DSL syntax!
      o.map { t: Store =>
        t.synchUp(s) // touch it up with most recent data if dirty
        t
      } openOr {
        val t = create(s)
        t.save
        t
      }
    }
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
    * Find the closest store by coordinates
    */
  def find( lat: String,  lon: String): Box[StoreAsLCBOJson] = synchronized {
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

  private def findStore(lat: String, lon: String): Box[StoreAsLCBOJson] = {
    val url = s"$LcboDomainURL/stores?where_not=is_dead" +
      additionalParam("lat", lat) +
      additionalParam("lon", lon)
      tryo { collectStoresOnAPage(List[StoreAsLCBOJson](), url, MaxSampleSize, pageNo = 1).head }
       // val stores = collectStoresOnAPage(List[StoreAsLCBOJson](), url, MaxSampleSize, pageNo = 1)
       // fetchSynched(stores.head)
      //}
  }

  @throws(classOf[net.liftweb.json.MappingException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @scala.annotation.tailrec
  private final def collectStoresOnAPage(accumItems: List[StoreAsLCBOJson],
                                 urlRoot: String,
                                 requiredSize: Int,
                                 pageNo: Int): List[StoreAsLCBOJson] = {
    val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo)
    logger.info(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = for (p <- itemNodes) yield p.extract[StoreAsLCBOJson]
    lazy val outstandingSize = requiredSize - items.size
    lazy val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extract[Boolean]

    if (items.isEmpty || outstandingSize <= 0 || isFinalPage) return accumItems ++ items

    collectStoresOnAPage(
      accumItems ++ items,
      urlRoot,
      outstandingSize,
      pageNo + 1) // union of this page with next page when we are asked for a full sample
  }

}