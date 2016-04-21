package code.model

import scala.collection.Iterable
import net.liftweb.common.{Box, Full, Loggable}

/**
  * Created by philippederome on 2016-04-03.
  */
trait ErrorReporter extends Loggable {
  // returns true on success, false with error on failure
  def checkUnitErrors(box: Box[Unit],
                      formatter: (String, String) => String): String =
    box match {
      case net.liftweb.common.Failure(m, ex, _) => formatter(m, ex.toString)
      case _ => ""
    }

  // false wit error on failure, true on success. Client expects that the box will contain at least one item and wants an error if there is 0 item.
  // This is not meant to be particularly reusable but there are two instances when it's convenient to log error on empty retrieval (first param box).
  def checkErrors(box: Box[Iterable[Any]],
                  formatter: (String, String) => String,
                  error: String) : String =
    box match {
      case net.liftweb.common.Failure(m, ex, _) => formatter(m, ex.toString)
      case Full(Nil) => error  // This is not None or Empty, just a normal result but empty Iterable.
      case _ => "" // 1 or more in iterable
    }

}
