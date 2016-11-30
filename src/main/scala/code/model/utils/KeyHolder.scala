package code.model.utils

import code.model.{Inventory, Product, Store}

/**
  * Created by philippederome on 2016-05-13.
  */

trait KeyHolder[A] {
  def getKey(f: A): String
}

object KeyHolder {
  implicit val inventoryHolder = new KeyHolder[Inventory] {
    def getKey(f: Inventory) = s"${f.productid}:${f.storeid}"
  }

  implicit val productHolder = new KeyHolder[Product] {
    def getKey(f: Product) = f.lcboKey.toString
  }
  implicit val storeHolder = new KeyHolder[Store] {
    def getKey(f: Store) = f.lcboKey.toString
  }
}

