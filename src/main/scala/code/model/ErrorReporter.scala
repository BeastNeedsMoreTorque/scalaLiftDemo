package code.model

import scala.collection.Iterable
import net.liftweb.common.{Box, Empty, Loggable}

/**
  * Created by philippederome on 2016-04-03.
  */
trait ErrorReporter extends Loggable {
  // returns true on success, false on failure
  def checkUnitErrors(box: Box[Unit],
                      contextErr: (String, String) => String): Boolean =
    box match {
      case net.liftweb.common.Failure(m, ex, _) =>
        val s: String = contextErr(m, ex.toString)
        logger.error(s)
        false
      case _ => true
    }

  // returns true on success, false on failure
  def checkErrors(box: Box[Iterable[Any]],
                  fullContextErr: (String, String) => String,
                  simpleErr: String) : Boolean =
    box match {
      case net.liftweb.common.Failure(m, ex, _) =>
        val s: String = fullContextErr(m, ex.toString)
        logger.error(s)
        false
      case Empty =>
        logger.error(simpleErr)
        false
      case _ => true
    }

}
