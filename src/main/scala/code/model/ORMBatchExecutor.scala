package code.model

import java.sql.SQLException

import net.liftweb.common.Loggable

import scala.collection.Iterable

/**
  * Created by philippederome on 2016-04-03. Just to isolate the verbose try catch block. Please, hide this try catch block from privy eyes.
  */
trait ORMBatchExecutor extends Loggable {
  def execute[T](items: Iterable[T], transactor: (Iterable[T]) => Unit): Unit = {
    try {
      transactor(items)
    } catch {
      case se: SQLException =>
        logger.error(s"SQLException $items")
        logger.error("Code: " + se.getErrorCode)
        logger.error("SqlState: " + se.getSQLState)
        logger.error("Error Message: " + se.getMessage)
        logger.error("NextException:" + se.getNextException)
      // intentionally skip other errors and let it go higher up.
    }
  }
}
