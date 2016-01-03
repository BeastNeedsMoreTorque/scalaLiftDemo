package code.snippet

import code.model.{Product, Store}
import net.liftweb.common.{Empty, Box}
import net.liftweb.http.SessionVar
import net.liftweb.util.Props

/**
  * Created by philippederome on 2015-12-05.
  */
object SessionCache {
  val defaultCategory = Props.get("product.Category", "wine")
  private val defaultStore = Props.getInt("store.Id", 1)                  // if 1 it's HWY 401 & Weston.

  object theCategory extends SessionVar[String](defaultCategory)
  object theStore extends SessionVar[Store](Store(id=defaultStore))
  object theProduct extends SessionVar[Box[Product]](Empty)
  object transactionConfirmation extends SessionVar[String]("")
}
