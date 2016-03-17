package code.model

import java.util.Calendar
import java.sql.SQLException

import code.Rest.RestClient
import net.liftweb.json._
import net.liftweb.util.Props

import scala.collection.{IndexedSeq, concurrent}
import scala.collection.mutable.ArrayBuffer

import net.liftweb.common.Loggable
import net.liftweb.json.JsonParser.parse
import net.liftweb.record.field.DateTimeField
import net.liftweb.record.{Record,MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._

import org.squeryl.Query
import org.squeryl.Table

/**
  * Created by philippederome on 2016-01-02. Credit Lift Cookbook.
  */

trait Created[T <: Created[T]] extends Record[T] {
  self: T =>
  val created: DateTimeField[T] = new DateTimeField(this) {
    override def defaultValue = Calendar.getInstance
  }
}

trait Updated[T <: Updated[T]] extends Record[T] {
  self: T =>

  val updated = new DateTimeField(this) {
    override def defaultValue = Calendar.getInstance
  }

  def onUpdate = this.updated(Calendar.getInstance)

}

trait CreatedUpdated[T <: Updated[T] with Created[T]] extends
Updated[T] with Created[T] {
  self: T =>
}

trait Persistable[T <: Persistable[T]] extends Record[T] with KeyedRecord[Long] with RestClient with Loggable {
  self: T =>

  def table(): Table[T]
  def cache(): concurrent.Map[Long, T]  // primary cache

  def meta: MetaRecord[T]
  def LcboIdsToDBIds(): concurrent.Map[Long, Long]  // secondary cache
  def pKey: Long
  def lcboId: Long
  def setLcboId(id: Long): Unit
  def batchSize: Int = 1024

  // Following values must be read as config externally. We don't mean to rely on defaults below, rather properties should be set sensibly.
  def HttpClientConnTimeOut = Props.getInt("http.ClientConnTimeOut", 5000)
  def HttpClientReadTimeOut = Props.getInt("http.ClientReadTimeOut", HttpClientConnTimeOut)
  def LcboDomainURL = Props.get("lcboDomainURL", "http://") // set it!

  /**
    * streams as String a parameter tag value pair prefixed with & as building block for a URL query String.
    * Assumes not first one (uses &). Improve! First approximation towards a better solution.
    * @param name name of optional parameter
    * @param value value of optional parameter
    * @return     &<name>=<value>         Assumes value is meaningful with printable data, but does not validate it.
    */
  def additionalParam[A](name: String, value: A): String = s"&$name=$value"

  implicit val formats = net.liftweb.json.DefaultFormats

  def init(xactLastStep: => (Iterable[T])=> Unit ): Unit = {
    // load all stores from DB for navigation and synch with LCBO for possible delta (small set so we can afford synching, plus it's done async way)
    inTransaction {
      val items = from(table())(s => select(s))
      addNewItemsToCaches(items)
      xactLastStep(items)
    }
  }

  def addNewItemsToCaches(items: Iterable[T]): Unit = {
    cache() ++= items.map{x => x.pKey -> x } (collection.breakOut)
    LcboIdsToDBIds ++= cache().map { case(k, v) => v.lcboId -> k }
  }

  def isFinalPage(jsonRoot: JValue, pageNo: Int): Boolean = {
    //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)
    isFinalPage || totalPages < pageNo + 1
  }

  def extractLcbo(urlRoot: String, maxPerPage: Int, pageNo: Int ): (IndexedSeq[T], Boolean, String) = {
    // specify the URI for the LCBO api url for liquor selection
    val uri = urlRoot + additionalParam("per_page", maxPerPage) + additionalParam("page", pageNo) // get as many as possible on a page because we could have few matches.
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val nodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
    // collect our list of products in items and filter out unwanted products

    val items = ArrayBuffer[T]()
    for (p <- nodes;
         key <- (p \ "id").extractOpt[Long];
         rec <- meta.fromJValue(p)) {
           rec.setLcboId(key) //hack. Record is forced to use "id" as read-only def, which means we cannot extract it direct... Because of PK considerations at Squeryl.
           items += rec
         }
    (items.toIndexedSeq, isFinalPage(jsonRoot, pageNo), uri)
  }

  // Always call update before insert just to be consistent and safe. Enforce it.
  def updateAndInsert(updateItems: Iterable[T], insertItems: IndexedSeq[T]): Unit = {
    update(updateItems)
    insert(insertItems)
  }

  // @see http://squeryl.org/occ.html
  private def update(items: Iterable[T]): Unit = {
    val t = table()

    items.grouped(batchSize).
      foreach { subItems =>
        var itemsWithPK: Query[T] = null
        val ids = items.map(_.pKey)
        inTransaction {
          try {
            t.forceUpdate(subItems) // @see http://squeryl.org/occ.html.
            itemsWithPK = from(t)(p => where( p.idField in ids) select(p))
          } catch {
            case se: SQLException =>
              logger.error(s"SQLException $subItems")
              logger.error("Code: " + se.getErrorCode)
              logger.error("SqlState: " + se.getSQLState)
              logger.error("Error Message: " + se.getMessage)
              logger.error("NextException:" + se.getNextException)
            case e: Exception =>
              logger.error("General exception caught: " + e+ " " + subItems)
          }
        }
        // regular call as update throws.
        // We don't care if two threads attempt to update the same product (from two distinct stores and one is a bit more stale than the other)
        // However, there are other situations where we might well care.
        if (itemsWithPK ne null) cache() ++= itemsWithPK.map{x => x.pKey -> x } (collection.breakOut)  // refresh from the database select not from data we sent down.
      }
  }

  private def insert( items: IndexedSeq[T]): Unit = {
    val t = table()

    // Do special handling to filter out duplicate keys, which would throw.
    def insertBatch(items: Iterable[T]): Unit = {
      // insert them
      var filteredProdsWithPKs: Query[T] = null
      val ids = items.map(_.lcboId)
      inTransaction {
        try {  // getNextException in catch is why we want to try catch here.
          // the DB could fail for PK or whatever other reason.
          // we require transaction for insert and following refresh select.
          t.insert(items) // refresh them with PKs assigned by DB server.
          filteredProdsWithPKs = from(t)(p => where( p.lcboId in ids) select(p))
        } catch {
          case se: SQLException =>
            logger.error(s"SQLException $items")
            logger.error("Code: " + se.getErrorCode)
            logger.error("SqlState: " + se.getSQLState)
            logger.error("Error Message: " + se.getMessage)
            logger.error("NextException:" + se.getNextException)
          case e: Exception =>
            logger.error("General exception caught: " + e)
        }
      }
      if (filteredProdsWithPKs ne null) addNewItemsToCaches(filteredProdsWithPKs)
    }
    // first evaluate against cache (assumed in synch with DB) what's genuinely new.
    val LcboIDs = cache().map{ case (id, p) => p.lcboId}.toSet // evaluate once
    val filteredForRI = items.filterNot { p => LcboIDs.contains(p.lcboId) }
    // you never know... Our input could have the same product twice in the collection with the same lcbo_id and we have unique index in DB against that.
    val filteredForUnique = filteredForRI.groupBy {_.lcboId}.
      map { case (k,v) => v.last }
    // break it down and then serialize the work.
    filteredForUnique.grouped(batchSize).foreach { insertBatch }
  }
}