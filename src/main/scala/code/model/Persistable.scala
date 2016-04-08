package code.model

import code.model.GlobalLCBO_IDs.LCBO_ID

import scala.collection.{IndexedSeq, Iterable}
import scala.collection.mutable.ArrayBuffer
import net.liftweb.util.Props
import net.liftweb.common.Loggable
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._

/**
  * Created by philippederome on 2016-03-17.
  */
trait Persistable[T <: Persistable[T]] extends Loader[T] with LCBOPageFetcher[T] with ItemStateGrouper
  with KeyedRecord[Long] with ORMBatchExecutor with propsSeqReader with Loggable {
  self: T=>

  def setLcboId(id: LCBO_ID): Unit

  val LcboExtract: JSitemsExtractor =  { nodes =>
    val items = ArrayBuffer[T]()
    for (p <- nodes;
         key <- (p \ "id").extractOpt[Long];
         rec <- meta.fromJValue(p)) {
      rec.setLcboId(LCBO_ID(key)) //hack. Record is forced to use "id" as read-only def, which means we cannot extract it direct... Because of PK considerations at Squeryl (KeyedEntity has def id: K, which is read-only).
      items += rec
    }
    items.toIndexedSeq
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

  private val batchSize: Int = Props.getInt("DBWrite.BatchSize", 1024)
  // following cannot be val because of table() usage and timing and need for underlying transaction, connection, etc.
  private def forceUpdater: (Iterable[T]) => Unit = table().forceUpdate _  // @see http://squeryl.org/occ.html. Regular call as update throws because of possibility of multiple updates on same record.
  private def insertBatch: (Iterable[T]) => Unit = table().insert _

  def loadToCacheLastTransaction(items: Iterable[T]) = {
    val itemsWithPK = from(table())(item => where( item.idField in items.map( _.pKey.underlying)) select(item))
    cache() ++= itemsWithPK.map{item => item.pKey -> item } (collection.breakOut)
  }

  def batchTransactor(items: Iterable[T], transactor: (Iterable[T]) => Unit) = {
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
