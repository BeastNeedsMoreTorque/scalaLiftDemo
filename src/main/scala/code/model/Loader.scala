package code.model

import scala.collection.{Iterable, concurrent}
import net.liftweb.record.Record
import net.liftweb.squerylrecord.RecordTypeMode._
import org.squeryl.Table

/**
  * Created by philippederome on 2016-03-23.
  */

trait Loader[T <: Loader[T]] extends Record[T]
{
  self: T =>

  protected def table(): Table[T]
  protected def cache(): concurrent.Map[Long, T]  // primary cache
  protected def LcboIdsToDBIds(): concurrent.Map[Long, Long]  // secondary cache

  protected def pKey: Long
  protected def lcboId: Long

  protected def cacheNewItems(items: Iterable[T]): Unit = {
    cache() ++= items.map{x => x.pKey -> x } (collection.breakOut)
    LcboIdsToDBIds() ++= cache().map { case(k, v) => v.lcboId -> k }
  }

  protected def load(): Unit = {
    // load all items from DB for navigation and synch with LCBO for possible delta (small set so we can afford synching, plus it's done async way)
    inTransaction {
      val items = from(table())(s => select(s))
      cacheNewItems(items)
    }
  }
}

