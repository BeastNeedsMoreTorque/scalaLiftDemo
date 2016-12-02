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
  implicit def storeShowKey: ShowKey[Store] = ShowKey( _.lcboKey.toString )
  implicit def invShowKey: ShowKey[Inventory] = ShowKey(inv => s"${inv.productid}:${inv.storeid}")
  implicit def prodShowKey: ShowKey[Product] = ShowKey( _.lcboKey.toString )
}

