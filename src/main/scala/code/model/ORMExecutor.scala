package code.model

import java.sql.SQLException

import net.liftweb.common.Loggable

import scala.collection.Iterable

/**
  * Created by philippederome on 2016-04-03.
  * Just to isolate the verbose try catch block.
  * The only spot we do a catch so far, BECAUSE we need all the diagnostics we can get
  * and we can identify the error neatly right here. Other error handling is done FP way with tryo/Box playing role of Either/Option.
  * Please, please, please: hide this try catch block from privy eyes. It makes me noxious. ;-)
  */
trait ORMExecutor extends Loggable {
  def execute[T](items: Iterable[T], apply: (Iterable[T]) => Unit): Unit = // captures exception in Box and force callers to deal with it.
    try {
      apply(items) // e.g. insert, update, delete
    } catch {
      case se: SQLException =>
        logger.error(s"SQLException $items")
        logger.error("Code: " + se.getErrorCode)
        logger.error("SqlState: " + se.getSQLState)
        logger.error("Error Message: " + se.getMessage)
        logger.error("NextException:" + se.getNextException)
        throw se
      // intentionally skip other errors and let them all be "handled" higher up including SQLException, record more context along the way.
      // actually typically immediately higher up because of the tryo returning a Box type (similar to Option/Either)
    }
}
