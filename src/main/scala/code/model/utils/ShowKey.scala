package code.model.utils

import code.model.{Inventory, Loader}
import code.model.GlobalLCBO_IDs._

// patterned after Show type class
trait ShowKey[A] { // it is meant to show a "key" from A, not all of A.
  def show(a: A): Long
}

object ShowKey {
  def apply[A](implicit s: ShowKey[A]): ShowKey[A] = s

  // SAM (Single Access Method, experimental in 2.11)
  def pure[A](f: A => Long): ShowKey[A] = { a => f(a) } // from Daniel Westheide talk on type classes

  implicit def showLoader[T <: Loader[T]]: ShowKey[T] = pure(_.lcboKey) // meets our needs for Store and Product
  implicit val invShowKey: ShowKey[Inventory] = pure(x => (32768 * x.store_id) + x.product_id) // here, the key is external key from LCBO
}