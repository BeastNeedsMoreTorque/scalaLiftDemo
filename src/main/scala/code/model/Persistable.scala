package code.model

import scala.collection.{IndexedSeq, Iterable}
import net.liftweb.util.Props
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._

/**
  * Created by philippederome on 2016-03-17. Unable to apply cake pattern here and prevent Store and Product to inherit from this,
  * so mitigate with access control on methods.
  */
trait Persistable[T <: Persistable[T]] extends Loader[T] with KeyedRecord[Long] with ORMBatchExecutor {
  self: T =>

  // Always call update before insert just to be consistent and safe. Enforce it.
  protected final def updateAndInsert(updateItems: Iterable[T], insertItems: IndexedSeq[T]): Unit = inTransaction {
    update(updateItems)
    insert(insertItems)
  }

  private val batchSize: Int = Props.getInt("DBWrite.BatchSize", 1024)
  // following cannot be val because of table() usage and timing and need for underlying transaction, connection, etc.
  private def forceUpdater: (Iterable[T]) => Unit = table().forceUpdate _  // @see http://squeryl.org/occ.html. Regular call as update throws because of possibility of multiple updates on same record.
  private def insertBatch: (Iterable[T]) => Unit = table().insert _

  private def loadToCacheLastTransaction(items: Iterable[T]) = {
    val pkIds = items.map(_.pKey: Long)  // type ascription to integrate with Squeryl below
    val itemsWithPK = from(table())(item => where(item.idField in pkIds) select(item))
    cache() ++= itemsWithPK.map{item => item.pKey -> item } (collection.breakOut)
  }

  private def batchTransactor(items: Iterable[T], transactor: (Iterable[T]) => Unit) = {
    execute[T](items, transactor)
    loadToCacheLastTransaction(items)
  }

  // We can afford to be less strict in our data preparation/validation than for the insert.
  private def update(items: Iterable[T]) =
    items.grouped(batchSize).
      foreach { batchTransactor( _ , forceUpdater) }

  private def insert( items: IndexedSeq[T]) = {
    // Do special handling to filter out duplicate keys, which would throw.
    val LcboIDs = from(table())(elt => select(elt.lcboId)).toSet // alas trust the database and not the cache, some other client could insert in database
    // (in fact, a surprising error occurred when trusting cache! Possibly a very subtle bug)

    // you never know... Our input could have the same item twice in the collection with the same lcbo_id and we have unique index in DB against that.
    items.
      filterNot { p => LcboIDs.contains(p.lcboId) }.  // prevent duplicate primary key for our current data in DB (considering LCBO ID as alternate primary key)
      groupBy {_.lcboId}.map { case (_, iseq) => iseq(0) }.  // remove duplicate lcboid keys from within our own input, selecting first representative among dupes!
      grouped(batchSize).foreach { batchTransactor( _ , insertBatch) } // break it down in reasonable size transactions, and then serialize the work.
  }
}
