package code.model

import java.sql.SQLException

import scala.collection.{IndexedSeq, Iterable}
import net.liftweb.util.Props
import net.liftweb.common.Loggable
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import org.squeryl.Query

/**
  * Created by philippederome on 2016-03-17.
  */
trait Persistable[T <: Persistable[T, I], I] extends Loader[T] with ItemStateGrouper[T, I] with KeyedRecord[Long] with Loggable {
  self: T =>

  def synchDirtyAndNewItems(items: IndexedSeq[T], getCachedItem: (T) => Option[I], dirtyPred: (I, T) => Boolean): Unit = {
    val (dirtyItems, newItems) = itemsByState(items, getCachedItem, dirtyPred)
    updateAndInsert(dirtyItems, newItems) // updates DB AND cache.
  }
  // Always call update before insert just to be consistent and safe. Enforce it.
  private def updateAndInsert(updateItems: Iterable[T], insertItems: IndexedSeq[T]): Unit = inTransaction {
    update(updateItems)
    insert(insertItems)
  }

  private def batchSize: Int = Props.getInt("DBWrite.BatchSize", 1024)

  // @see http://squeryl.org/occ.html
  private def update(items: Iterable[T]) = {
    val t = table()

    items.grouped(batchSize).
      foreach { subItems =>
        var itemsWithPK: Query[T] = null
        val ids = items.map(_.pKey)
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

        // regular call as update throws.
        // We don't care if two threads attempt to update the same product (from two distinct stores and one is a bit more stale than the other)
        // However, there are other situations where we might well care.
        if (itemsWithPK ne null) cache() ++= itemsWithPK.map{x => x.pKey -> x } (collection.breakOut)  // refresh from the database select not from data we sent down.
      }
  }

  private def insert( items: IndexedSeq[T]) = {
    val t = table()

    // Do special handling to filter out duplicate keys, which would throw.
    def insertBatch(items: Iterable[T]): Unit = {
      // insert them
      var filteredProdsWithPKs: Query[T] = null
      val ids = items.map(_.lcboId)
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
