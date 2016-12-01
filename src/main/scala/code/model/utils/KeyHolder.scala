package code.model.utils

import code.model.{Inventory, Store, Product}

trait KeyHolder[A] { // effectively resembles Show[A] a great deal, but is meant to show a small aspect of the type A, not the whole thing.
  def getKey(f: A): String
}

object KeyHolder {
  def getKey[A](f: A => String): KeyHolder[A] = new KeyHolder[A] {
    def getKey(a: A): String = f(a)
  }
  implicit val storekeyHolder: KeyHolder[Store] = KeyHolder.getKey { s => s.lcboKey.toString }
  implicit val keyInventoryHolder: KeyHolder[Inventory] = KeyHolder.getKey { inv => s"${inv.productid}:${inv.storeid}" }
  implicit val keyProductHolder: KeyHolder[Product] = KeyHolder.getKey { p => p.lcboKey.toString }
}

