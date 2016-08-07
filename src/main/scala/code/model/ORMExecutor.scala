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
        val err = s"""SQLException $items
          Code: ${se.getErrorCode}
          SqlState: ${se.getSQLState}
          Error Message: ${se.getMessage}
          NextException: ${se.getNextException}"""
        Xor.Left(err)
      case other: Throwable =>
        Xor.Left(other.toString())
    }
}
