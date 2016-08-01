package code.model

import java.sql.SQLException
import cats.data.Xor
import scala.collection.Iterable

/**
  * Created by philippederome on 2016-04-03.
  * Just to isolate the verbose try catch block.
  * The only spot we do a catch so far, BECAUSE we need all the diagnostics we can get
  * and we can identify the error neatly right here.
  */
trait ORMExecutor {
  def execute[T](items: Iterable[T], apply: (Iterable[T]) => Unit): Xor[String, Iterable[T]] =
    try {
      apply(items)
      Xor.Right(items) // e.g. insert, update, delete
    } catch {
      case se: SQLException =>
        val err = s"SQLException $items " +
          s"Code: ${se.getErrorCode} " +
          s"SqlState: ${se.getSQLState} " +
          s"Error Message: ${se.getMessage} " +
          s"NextException: ${se.getNextException}"
        Xor.Left(err)
      case other: Throwable =>
        Xor.Left(other.toString())
    }
}
