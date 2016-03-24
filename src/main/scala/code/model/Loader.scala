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

  def table(): Table[T]
  def cache(): concurrent.Map[Long, T]  // primary cache
  def LcboIdsToDBIds(): concurrent.Map[Long, Long]  // secondary cache

  def pKey: Long
  def lcboId: Long

  def cacheNewItems(items: Iterable[T]): Unit = {
    cache() ++= items.map{x => x.pKey -> x } (collection.breakOut)
    LcboIdsToDBIds() ++= cache().map { case(k, v) => v.lcboId -> k }
  }

  def load(): Iterable[T] = {
    // load all stores from DB for navigation and synch with LCBO for possible delta (small set so we can afford synching, plus it's done async way)
    inTransaction {
      val items = from(table())(s => select(s))
      cacheNewItems(items)
      items
    }
  }
}

