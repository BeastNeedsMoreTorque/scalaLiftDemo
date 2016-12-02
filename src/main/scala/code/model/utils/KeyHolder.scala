package code.model.utils

import code.model.{Inventory, Store, Product}

trait KeyHolder[A] { // effectively resembles Show[A] a great deal, but is meant to show a small aspect of the type A, not the whole thing.
  def getKey(f: A): String
}

object KeyHolder {
  def apply[A](f: A => String): KeyHolder[A] = new KeyHolder[A] {
    def getKey(a: A): String = f(a)
  }
  implicit def storekeyHolder: KeyHolder[Store] = KeyHolder { s => s.lcboKey.toString }
  implicit def keyInventoryHolder: KeyHolder[Inventory] = KeyHolder { inv => s"${inv.productid}:${inv.storeid}" }
  implicit def keyProductHolder: KeyHolder[Product] = KeyHolder { p => p.lcboKey.toString }
}

