package code.model.utils

import code.model.{Inventory, Loader}
import code.model.GlobalLCBO_IDs._

// patterned after Show type class
trait ShowKey[A] { // it is meant to show a "key" from A, not all of A.
  def show(a: A): Long
}

object ShowKey {
  def apply[A](f: A => Long): ShowKey[A] = new ShowKey[A] {
    def show(a: A): Long = f(a)
  }

  // SAM (Single Access Method, experimental in 2.11)
  def pure[A](f: A => Long): ShowKey[A] = { a => f(a) } // from Daniel Westheide talk on type classes

  implicit def showLoader[T <: Loader[T]]: ShowKey[T] = pure( _.lcboKey )
  implicit val invShowKey: ShowKey[Inventory] = pure(_.hashCode) // here, the key is all the data of the case class
}
