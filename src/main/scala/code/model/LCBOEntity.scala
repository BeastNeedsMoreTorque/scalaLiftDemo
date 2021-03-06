package code.model

import scala.collection.{Seq, IndexedSeq, Iterable}
import code.model.utils.RetainSingles._
import code.model.utils.ShowKey
import code.model.GlobalLCBO_IDs._
import net.liftweb.record.field.{OptionalStringField, StringField}
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props

/**
  * Created by philippederome on 2016-04-10. Highest level trait to share between Product and Store that have much logic in common.
  * @see F-bounded polymorphism
  */
trait LCBOEntity[T <: LCBOEntity[T]] extends Loader[T] with KeyedRecord[Long] with CreatedUpdated[T] {
  self: T =>

  // Always call update before insert just to be consistent and safe. Enforce it.
  protected final def updateAndInsert(updateItems: Iterable[T], insertItems: IndexedSeq[T])
                                     (implicit ev: ShowKey[T]): Unit = inTransaction {
    update(updateItems)
    insert(insertItems)
  }

  private val batchSize = Props.getInt("DBWrite.BatchSize", 1)

  // We can afford to be less strict in our data preparation/validation than for the insert.
  private def update(items: Iterable[T]) = {
    // @see http://squeryl.org/occ.html. Regular call as update throws because of possibility of multiple updates on same record.
    def ormUpdater: Iterable[T] => Unit = table.forceUpdate _
    items.grouped(batchSize).
      foreach {
        batchTransactor( _ , ormUpdater)
      }
  }

  private def insert(items: IndexedSeq[T])(implicit ev: ShowKey[T]) = {
    // following cannot be val because of table() usage and timing and need for underlying transaction, connection, etc.
    def ormInserter: Iterable[T] => Unit = table.insert _

    // Do special handling to filter out duplicate keys, which would throw.
    // Trust the database and not the cache, some other client could insert in database
    val lcboKeys = from(table)(item => select(item.lcboKey)).toSet

    // removes any duplicate keys and log error if found duplicates
    // prevent duplicate primary key for our current data in DB (considering LCBO ID as alternate primary key)
    val filtered = retainSingles(items).filterNot( p => lcboKeys(p.lcboKey) )
    // break it down in reasonable size transactions, and then serialize the work.
    filtered.grouped(batchSize).foreach { batchTransactor( _ , ormInserter) }
  }

  private def batchTransactor(items: Iterable[T], ORMTransactor: Iterable[T] => Unit): Unit = {
    def feedCache(items: Iterable[T]) = {
      val akIds = items.map(_.lcboKey: Long).toStream.distinct // alternate key ids, which are a proxy for our primary key IDs to be evaluated from DB.
      // select only those we just inserted, hopefully the same set (cardinality of akIds is expected to be smaller than items
      // because of repetition at the source).
      val itemsWithAK = from(table)(item => where((item.lcboKey: Long) in akIds) select item)
      if (itemsWithAK.size < akIds.size) logger.error(s"feedCache got only ${itemsWithAK.size} items for expected ${akIds.size}")
      cacheItems(itemsWithAK)
    }

    // Seems high level enough in code to report error to log as it appears a little messy to push it up further.
    lazy val context = (err: String) =>
      s"Problem with batchTransactor, exception error $err"

    ORMExecutor.execute(ORMTransactor, items).
      fold(err => logger.error(context(err)), (Unit) => feedCache(items))
  }
  /**
    * An optional StringField of given maxLength with empty string as default and a default filter that works for us:
    * meaning keep string small don't pad with spaces (crop) and replace null String with empty string.
    * @param maxLength beyond this maxLength, we are ready to lose data without error
    */
  class FilteredOptionalStringField(maxLength: Int) extends OptionalStringField(this, maxLength) {
    type filterMethodsList = List[(FilteredOptionalStringField.this.ValueType) => FilteredOptionalStringField.this.ValueType]
    override def defaultValue: String = emptyString
    override def setFilter: filterMethodsList = notNull _ :: crop _ :: super.setFilter
    def getValue: String = _1.toOption.fold(defaultValue)(identity)
  }

  /**
    * A mandatory StringField of given maxLength with empty string as default and a default filter that works for us:
    * meaning keep string small don't pad with spaces (crop) and replace null String with empty string.
    * @param maxLength beyond this maxLength, we are ready to lose data without error
    */
  class FilteredMandatoryStringField(maxLength: Int) extends StringField(this, maxLength) {
    type filterMethodsList = List[(FilteredMandatoryStringField.this.ValueType) => FilteredMandatoryStringField.this.ValueType]
    override def defaultValue: String = emptyString
    override def setFilter: filterMethodsList = notNull _ :: crop _ :: super.setFilter
  }

  /**
    * represents a map of String to Map(key:String, value:String) that is configurable at run time.
    * @param masterKey a key to some configuration holding a map of values
    * @return a map of strings to strings bound to the provided masterKey
    */
  def getSeq(masterKey: String): Seq[(String, String)] =
    ConfigPairsRepo.getSeq(masterKey)

  /**
    * type parameter I is extended by T, so that get can return an interface rather than a concrete class
    * Some LCBO entities also have a similar pattern of identifying new info from LCBO (items being provided from a query),
    * reconciling/interpreting as new or dirty (or clean/unchanged)
    * and then make sure first DB is brought up to date with that info and synchronously the cache memory as well.
    */
  final def synchDirtyAndNewItems[I >: T](items: IndexedSeq[T], get: I => Option[I])(implicit ev: ShowKey[T]): IndexedSeq[T] = {
    val dirtyAndNewItems = ItemStateGrouper.itemsByState[I, T](items, get)
    updateAndInsert(dirtyAndNewItems.updates, dirtyAndNewItems.inserts) // updates DB AND cache.
    items
  }
  private def emptyString = ""
}
