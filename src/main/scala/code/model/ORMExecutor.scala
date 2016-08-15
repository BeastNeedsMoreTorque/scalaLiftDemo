package code.model

import java.sql.SQLException
import cats.data.Xor

/**
  * Created by philippederome on 2016-04-03.
  * Just to isolate the verbose try catch block.
  * The only spot we do a catch so far, BECAUSE we need all the diagnostics we can get
  * and we can identify the error neatly right here.
  */
trait ORMExecutor {
  def execute[A, F[_]](f: F[A] => Unit, fa: F[A]): Xor[String, F[A]] =
    try {
      f(fa) // side effect: e.g. insert, update, delete
      Xor.Right(fa) // echo back the same data
    } catch {
      case se: SQLException =>
        val err = s"""SQLException $fa
          Code: ${se.getErrorCode}
          SqlState: ${se.getSQLState}
          Error Message: ${se.getMessage}
          NextException: ${se.getNextException}"""
        Xor.Left(err)
      case scala.util.control.NonFatal(other) =>
        Xor.Left(other.toString())
        // bubble up fatal ones
    }
}
