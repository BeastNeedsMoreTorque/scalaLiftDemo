package code.model

import scala.collection.{Iterable, concurrent}
import net.liftweb.record.Record
import net.liftweb.squerylrecord.RecordTypeMode._
import org.squeryl.Table
import code.model.GlobalLCBO_IDs.{LCBO_KEY, P_KEY}
import code.model.utils.RetainSingles.asMap
import net.liftweb.common.Loggable

/**
  * Created by philippederome on 2016-03-23.
  * Extends reading from a table for a record by associating the table with a cache
  * @tparam T represents the entity we can select for and cache for (e.g. Product)
  */
trait Loader[T <: Loader[T]] extends Record[T] with Loggable
{
  self: T =>

  /**
    * Underlying Squeryl ORM table
    * @return
    */
  protected def table: Table[T]

  /**
    *
    * @return  Primary cache by primary key
    */
  protected def cache: concurrent.Map[P_KEY, T]

  /**
    *
    * @return index by LCBO key
    */
  protected def lcboKeyToPK: concurrent.Map[LCBO_KEY, P_KEY]

  /**
    *
    * @return primary key
    */
  def pKey: P_KEY

  /**
    *
    * @return alternate key, LCBO's key
    */
  def lcboKey: LCBO_KEY

  /**
    * caches items
    * @param items items to be cached that have just been loaded
    */
  protected def cacheItems(items: Iterable[T]): Unit = {
    cache ++= asMap(items, {item: T => item.pKey})
    lcboKeyToPK ++= cache.map { case(pk, item) => item.lcboKey -> pk }
  }

  /**
    * reads from table and cache results
    */
  def load(): Unit = inTransaction {
    logger.info("load start")
    // load all items from DB for navigation and synch with LCBO for possible delta (small set so we can afford synching, plus it's done async way)
      val items = from(table)(s => select(s))
      cacheItems(items)
    logger.info("load end")
  }
}

