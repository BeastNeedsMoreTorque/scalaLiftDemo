package code.model

import scala.collection.Iterable
import net.liftweb.common.{Box, Empty, Full, Loggable}

/**
  * Created by philippederome on 2016-04-03.
  */
trait ErrorReporter extends Loggable {
  // returns true on success, false with error on failure
  def checkUnitErrors(box: Box[Unit],
                      contextErr: (String, String) => String): (Boolean, String) =
    box match {
      case net.liftweb.common.Failure(m, ex, _) =>
        (false, contextErr(m, ex.toString))
      case _ => (true, "")
    }

  // false wit error on failure, true on success. Client expects that the box will contain at least one item and wants an error if there is 0 item.
  def checkErrors(box: Box[Iterable[Any]],
                  fullContextErr: (String, String) => String,
                  simpleErr: String) : (Boolean, String) =
    box match {
      case net.liftweb.common.Failure(m, ex, _) =>
        (false, fullContextErr(m, ex.toString))
      case Full(Nil) => (false, simpleErr)  // This is not None or Empty, just a normal result but empty Iterable.
      case Empty => (false, simpleErr)  // This is None or Empty
      case _ => (true, "") // 1 or more in iterable
    }

}
