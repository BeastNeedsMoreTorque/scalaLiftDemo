package code.model

import scala.collection.{Iterable, concurrent}
import net.liftweb.record.Record
import net.liftweb.squerylrecord.RecordTypeMode._
import org.squeryl.Table
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.utils.RetainSingles
import net.liftweb.common.Loggable

/**
  * Created by philippederome on 2016-03-23.
  */

trait Loader[T <: Loader[T]] extends Record[T] with Loggable
{
  self: T =>

  protected def table(): Table[T]
  protected def cache(): concurrent.Map[P_KEY, T]  // primary cache
  protected def LcboIdToPK(): concurrent.Map[LCBO_ID, P_KEY]  // secondary cache

  protected def pKey: P_KEY
  protected def lcboId: LCBO_ID

  protected def cacheItems(items: Iterable[T]): Unit = {
    cache() ++= RetainSingles.asMap(items, {item: T => item.pKey})
    LcboIdToPK() ++= cache().map { case(pk, item) => item.lcboId -> pk }
  }

  def load(): Unit = inTransaction {
    logger.info("load start")
    // load all items from DB for navigation and synch with LCBO for possible delta (small set so we can afford synching, plus it's done async way)
      val items = from(table())(s => select(s))
      cacheItems(items)
    logger.info("load end")
  }
}

