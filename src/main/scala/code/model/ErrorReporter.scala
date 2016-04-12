package code.model

import scala.collection.Iterable
import net.liftweb.common.{Box, Empty, Full, Loggable}

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

  // log on failure, true on success. Client expects that the box will contain at least one item and wants an error if there is 0 item.
  def checkErrors(box: Box[Iterable[Any]],
                  fullContextErr: (String, String) => String,
                  simpleErr: String) : Boolean =
    box match {
      case net.liftweb.common.Failure(m, ex, _) =>
        logger.error(fullContextErr(m, ex.toString)); false
      case Full(Nil) => logger.error(simpleErr); false  // This is not None or Empty, just a normal result but empty Iterable.
      case _ => true
    }

}
