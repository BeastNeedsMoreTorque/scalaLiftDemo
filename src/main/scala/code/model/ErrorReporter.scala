package code.model

import scala.collection.Iterable
import net.liftweb.common.{Box, Empty, Loggable}

/**
  * Created by philippederome on 2016-04-03.
  */
trait ErrorReporter extends Loggable {
  // returns true on success, false on failure, log as side effect
  def checkUnitErrors(box: Box[Unit],
                      contextErr: (String, String) => String): Boolean =
    box match {
      case net.liftweb.common.Failure(m, ex, _) =>
        logger.error(contextErr(m, ex.toString)); false
      case _ => true
    }

  // log on failure, true on success
  def checkErrors(box: Box[Iterable[Any]],
                  fullContextErr: (String, String) => String,
                  simpleErr: String) : Boolean =
    box match {
      case net.liftweb.common.Failure(m, ex, _) =>
        logger.error(fullContextErr(m, ex.toString)); false
      case Empty => logger.error(simpleErr); false
      case _ => true
    }

}
