package code.model.utils

import code.model.{Inventory, Store, Product}

// patterned after Show type class
trait ShowKey[A] { // it is meant to show a "key" from A, not all of A.
  def show(a: A): String
}

object ShowKey {
  def apply[A](f: A => String): ShowKey[A] = new ShowKey[A] {
    def show(a: A): String = f(a)
  }

  // SAM (Single Access Method, experimental in 2.11)
  implicit val storeShowKey: ShowKey[Store] =  { _.lcboKey.toString }
  implicit val invShowKey: ShowKey[Inventory] = { (inv => s"${inv.productid}:${inv.storeid}") }
  implicit val prodShowKey: ShowKey[Product] = { _.lcboKey.toString }
}

