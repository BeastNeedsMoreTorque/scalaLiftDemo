package code.model

import java.util.Calendar
import java.sql.SQLException

import net.liftweb.common.Loggable
import net.liftweb.record.field.DateTimeField
import net.liftweb.record.Record
import net.liftweb.squerylrecord.KeyedRecord

import net.liftweb.squerylrecord.RecordTypeMode._

import scala.collection.concurrent

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

trait Persistable[T <: Persistable[T]] extends Record[T] with KeyedRecord[Long] with Loggable {
  self: T =>

  def table(): org.squeryl.Table[T]
  def cache(): concurrent.Map[Long, T]  // primary cache
  def LcboIdsToDBIds(): concurrent.Map[Long, Long]
  def pKey: Long
  def lcboId: Long

  def batchSize: Int = 1024

  def addNewItemsToCaches(items: Iterable[T]): Unit = {
    cache ++= items.map{x => x.pKey -> x } (collection.breakOut)
    LcboIdsToDBIds ++= cache.map { case(k,v) => v.lcboId -> k }
  }

  // @see http://squeryl.org/occ.html
  def update(items: Iterable[T]): Unit = meta.synchronized {
    val t = table()

    items.grouped(batchSize).
      foreach { subItems =>
        try {
          inTransaction { t.forceUpdate(subItems) } // @see http://squeryl.org/occ.html.
          // regular call as update throws.
          // We don't care if two threads attempt to update the same product (from two distinct stores and one is a bit more stale than the other)
          // However, there are other situations where we might well care.
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
        // update in memory for next caller who should be blocked (updateProducts is synchronized, helping DB and cache to be in synch)
        cache ++= subItems.map { x => x.pKey -> x } (collection.breakOut)
      }
  }

  def insert( items: IndexedSeq[T]): Unit = {
    val t = table()

    // Do special handling to filter out duplicate keys, which would throw.
    def insertBatch(items: Iterable[T]): Unit = meta.synchronized { // synchronize on LCBO object/singleton as clients are from different threads
      // insert them
      try {  // getNextException in catch is why we want to try catch here.
        // the DB could fail for PK or whatever other reason.
        inTransaction { t.insert(items) } // refresh them with PKs assigned by DB server.
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
      val ids = items.map(_.lcboId)
      val filteredProdsWithPKs = from(t)(p => where( p.lcboId in ids) select(p))
      // update in memory for next caller who should be blocked
      addNewItemsToCaches(filteredProdsWithPKs)
    }
    // first evaluate against cache (assumed in synch with DB) what's genuinely new.
    val LcboIDs = cache.map{ case (id, p) => p.lcboId}.toSet // evaluate once
    val filteredForRI = items.filterNot { p => LcboIDs.contains(p.lcboId) }
    // you never know... Our input could have the same product twice in the collection with the same lcbo_id and we have unique index in DB against that.
    val filteredForUnique = filteredForRI.groupBy {_.lcboId}.
      map { case (k,v) => v.last }
    // break it down and then serialize the work.
    filteredForUnique.grouped(batchSize).foreach { insertBatch }
  }
}