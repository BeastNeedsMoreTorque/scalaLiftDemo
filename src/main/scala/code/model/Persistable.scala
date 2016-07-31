package code.model

import scala.collection.{IndexedSeq, Iterable}
import net.liftweb.util.Props
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import code.model.utils.RetainSingles.implicitSeqToRetainSingles

/**
  * Created by philippederome on 2016-03-17. Unable to apply cake pattern here and prevent Store and Product to inherit from this,
  * so mitigate with access control on methods, one method is protected.
  */
trait Persistable[T <: Persistable[T]] extends Loader[T] with KeyedRecord[Long] with ORMExecutor with KeyKeeper {
  self: T =>

  // Always call update before insert just to be consistent and safe. Enforce it.
  protected final def updateAndInsert(updateItems: Iterable[T], insertItems: IndexedSeq[T]): Unit = inTransaction {
    update(updateItems) // in a Kafka world, this should be an insert with a new version (log append idea)
    insert(insertItems)
  }

  private val batchSize = Props.getInt("DBWrite.BatchSize", 1024)

  // We can afford to be less strict in our data preparation/validation than for the insert.
  private def update(items: Iterable[T]) = {
    // @see http://squeryl.org/occ.html. Regular call as update throws because of possibility of multiple updates on same record.
    def ormUpdater: Iterable[T] => Unit = table.forceUpdate _
    items.grouped(batchSize).
      foreach {
        batchTransactor( _ , ormUpdater)
      }
  }

  private def insert(items: IndexedSeq[T]) = {
    // following cannot be val because of table() usage and timing and need for underlying transaction, connection, etc.
    def ormInserter: Iterable[T] => Unit = table.insert _

    // Do special handling to filter out duplicate keys, which would throw.
    // Trust the database and not the cache, some other client could insert in database
    val LcboIDs = from(table)(item => select(item.lcboId)).toSet

    // removes any duplicate keys and log error if found duplicates
    // prevent duplicate primary key for our current data in DB (considering LCBO ID as alternate primary key)
    val filtered = items.retainSingles.filterNot( p => LcboIDs(p.lcboId) )
    // break it down in reasonable size transactions, and then serialize the work.
    filtered.grouped(batchSize).foreach { batchTransactor( _ , ormInserter) }
  }

  private def batchTransactor(items: Iterable[T], ORMTransactor: Iterable[T] => Unit): Unit = {
    def feedCache(items: Iterable[T]) = {
      val akIds = items.map(_.lcboId: Long).toStream.distinct // alternate key ids, which are a proxy for our primary key IDs to be evaluated from DB.
      // select only those we just inserted, hopefully the same set (cardinality of akIds is expected to be smaller than items
      // because of repetition at the source).
      val itemsWithAK = from(table)(item => where(item.lcboId.underlying in akIds) select item)
      if (itemsWithAK.size < akIds.size) logger.error(s"feedCache got only ${itemsWithAK.size} items for expected ${akIds.size}")
      cacheItems(itemsWithAK)
    }

    // Seems high enough to report error to log as it appears a little messy to push it up further.
    val context = (err: String) =>
      s"Problem with batchTransactor, exception error $err"

    val executedOrErrors = execute[T](items, ORMTransactor)
    executedOrErrors.fold[Unit](err => logger.error(context(err)), feedCache)
  }
}
