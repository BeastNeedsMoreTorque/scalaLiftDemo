package code.model

import java.sql.SQLException
import language.higherKinds
/**
  * Created by philippederome on 2016-04-03.
  * Offers mechanism to report errors as SQLException with some detail
  */
trait ORMExecutor {
  /**
    * executes what is assumed to be a database operation using f for a collection of items fa
    * formatting error message in special way for details if it is a SQLException
    * @param f a database operation with side effect, e.g. insert, update, delete
    * @param fa collection of items to be sent to a database
    * @tparam A underlying type of elements to persist
    * @tparam F type of container of elements
    * @return error message in Left of Xor or Right of Unit if all is well.
    */
  def execute[A, F[_]](f: F[A] => Unit, fa: F[A]): Either[String, Unit] =
    try {
      f(fa)
      Right(Unit)
    } catch {
      case se: SQLException =>
        val err = s"""SQLException $fa
          Code: ${se.getErrorCode}
          SqlState: ${se.getSQLState}
          Error Message: ${se.getMessage}
          NextException: ${se.getNextException}"""
        Left(err)
      case scala.util.control.NonFatal(other) =>
        Left(other.toString())
        // bubble up fatal ones
    }
}
