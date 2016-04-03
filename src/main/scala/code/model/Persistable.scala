package code.model

import java.sql.SQLException

import scala.collection.{IndexedSeq, Iterable}
import net.liftweb.util.Props
import net.liftweb.common.Loggable
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._

/**
  * Created by philippederome on 2016-03-17.
  */
trait Persistable[T <: Persistable[T]] extends Loader[T] with ItemStateGrouper with KeyedRecord[Long] with Loggable {
  self: T=>

  val fullContextErr: (String) => ( String, String) => String = { collName => (m, err) =>
    s"Problem loading $collName into cache for '$lcboId' with message $m and exception error $err"
  }
  val briefContextErr: (String) => () => String = { collName => () =>
    s"Problem loading $collName into cache for '$lcboId'"
  }

  // I should be an interface of T, so that getCachedItem can return an interface rather than a concrete class, and it should not return just anything.
  def synchDirtyAndNewItems[I >: T](items: IndexedSeq[T], getCachedItem: (I) => Option[I], dirtyPred: (I, T) => Boolean): Unit = {
    val (dirtyItems, newItems) = itemsByState[I, T](items, getCachedItem, dirtyPred)
    updateAndInsert(dirtyItems, newItems) // updates DB AND cache.
  }
  // Always call update before insert just to be consistent and safe. Enforce it.
  private def updateAndInsert(updateItems: Iterable[T], insertItems: IndexedSeq[T]): Unit = inTransaction {
    update(updateItems)
    insert(insertItems)
  }

  private def batchSize: Int = Props.getInt("DBWrite.BatchSize", 1024)

  // @see http://squeryl.org/occ.html. We can afford to be less strict in our data preparation/validation than for the insert.
  private def update(items: Iterable[T]) = {
    items.grouped(batchSize).
      foreach { batchedItems =>
        try {
          table().forceUpdate(batchedItems) // @see http://squeryl.org/occ.html. Regular call as update throws because of possibility of multiple updates on same record.
          val itemsWithPK = from(table())(item => where( item.idField in items.map(_.pKey)) select(item))
          cache() ++= itemsWithPK.map{x => x.pKey -> x } (collection.breakOut)
        } catch {
          case se: SQLException =>
            logger.error(s"SQLException $batchedItems")
            logger.error("Code: " + se.getErrorCode)
            logger.error("SqlState: " + se.getSQLState)
            logger.error("Error Message: " + se.getMessage)
            logger.error("NextException:" + se.getNextException)
          // intentionally skip other errors and let it go higher up.
        }
      }
  }

  private def insert( items: IndexedSeq[T]) = {
    // Do special handling to filter out duplicate keys, which would throw.
    def insertBatch(items: Iterable[T]): Unit = {
      // insert them
      try {  // getNextException in catch is why we want to try catch here.
        // the DB could fail for PK or whatever other reason.
        table().insert(items) // refresh them with PKs assigned by DB server.
        val filteredProdsWithPKs = from(table())(item => where( item.lcboId in items.map(_.lcboId)) select(item))
        cacheNewItems(filteredProdsWithPKs)
      } catch {
        case se: SQLException =>
          logger.error(s"SQLException $items")
          logger.error("Code: " + se.getErrorCode)
          logger.error("SqlState: " + se.getSQLState)
          logger.error("Error Message: " + se.getMessage)
          logger.error("NextException:" + se.getNextException)
        // intentionally skip other serious errors, and let them bubble up.
      }
    }
    // first evaluate against cache (assumed in synch with DB) what's genuinely new.
    val LcboIDs = cache().map{ case (id, p) => p.lcboId}.toSet // evaluate once
    // you never know... Our input could have the same product twice in the collection with the same lcbo_id and we have unique index in DB against that.
    items.
        filterNot { p => LcboIDs.contains(p.lcboId) }.  // prevent duplicate primary key for our current data in DB (considering LCBO ID as alternate primary key)
        groupBy {_.lcboId}.map { case (k,item) => item.last }.  // remove duplicates from within our own input, selecting last representative among dupes!
        grouped(batchSize).foreach { insertBatch } // break it down in reasonable size transactions, and then serialize the work.
  }
}
