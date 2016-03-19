package code.model

import java.net.URLEncoder
import java.sql.SQLException

import scala.collection.{IndexedSeq, concurrent}
import scala.collection.mutable.ArrayBuffer
import net.liftweb.util.Props
import net.liftweb.common.Loggable
import net.liftweb.json.JValue
import net.liftweb.json.JsonParser._
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import org.squeryl.{Query, Table}
import code.Rest.RestClient


/**
  * Created by philippederome on 2016-03-17.
  */
trait Persistable[T <: Persistable[T]] extends Record[T] with KeyedRecord[Long] with RestClient with Loggable {
  self: T =>

  def table(): Table[T]
  def cache(): concurrent.Map[Long, T]  // primary cache

  def meta: MetaRecord[T]
  def LcboIdsToDBIds(): concurrent.Map[Long, Long]  // secondary cache
  def pKey: Long
  def lcboId: Long
  def setLcboId(id: Long): Unit
  def batchSize: Int = Props.getInt("DBWrite.BatchSize", 1024)

  implicit val formats = net.liftweb.json.DefaultFormats

  def init(xactLastStep: => (Iterable[T])=> Unit ): Unit = {
    // load all stores from DB for navigation and synch with LCBO for possible delta (small set so we can afford synching, plus it's done async way)
    inTransaction {
      val items = from(table())(s => select(s))
      cacheNewItems(items)
      xactLastStep(items)
    }
  }

  def cacheNewItems(items: Iterable[T]): Unit = {
    cache() ++= items.map{x => x.pKey -> x } (collection.breakOut)
    LcboIdsToDBIds() ++= cache().map { case(k, v) => v.lcboId -> k }
  }

  def buildUrl(urlRoot: String, params: Seq[(String, Any)] ): String = {
    val encoding = "UTF-8"
    urlRoot + (params.map(v => URLEncoder.encode(v._1, encoding) + "=" + URLEncoder.encode(v._2.toString, encoding)).mkString("&")
                           match {case s if s.length == 0 => ""; case s => "?" + s})
  }

  def extractLcboItems(urlRoot: String, params: Seq[(String, Any)], pageNo: Int, maxPerPage: Int): (IndexedSeq[T], JValue, String) = {
    // specify the URI for the LCBO api url for liquor selection
    val fullParams = params ++ Seq(("per_page", maxPerPage), ("page", pageNo)) // get as many as possible on a page because we could have few matches.
    val uri = buildUrl(urlRoot, fullParams)
    val pageContent = get(uri) // fyi: throws IOException or SocketTimeoutException
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
    (items.toIndexedSeq, jsonRoot, uri)
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
      if (filteredProdsWithPKs ne null) cacheNewItems(filteredProdsWithPKs)
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
