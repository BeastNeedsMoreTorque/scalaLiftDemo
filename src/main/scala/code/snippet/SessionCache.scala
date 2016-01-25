package code.snippet

import code.model.Product
import net.liftweb.common.{Empty, Box}
import net.liftweb.http.SessionVar
import net.liftweb.util.Props

/**
  * Created by philippederome on 2015-12-05.
  */
object SessionCache {
  val defaultCategory = Props.get("product.Category", "wine")
  val defaultStore = 0                  // invalid on purpose.

  object theCategory extends SessionVar[String](defaultCategory)
  object theStoreId extends SessionVar[Int](defaultStore)
  object theProduct extends SessionVar[Box[Product]](Empty)
  object theProductInventory extends SessionVar[Int](0)

  object transactionConfirmation extends SessionVar[String]("")
}
