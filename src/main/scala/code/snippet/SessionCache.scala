package code.snippet

import code.model.Store
import net.liftweb.http.SessionVar
import net.liftweb.util.Props

/**
  * Created by philippederome on 2015-12-05.
  */
object SessionCache {
  private val defaultCategory = Props.get("product.Category", "wine")
  private val defaultStore = Props.getInt("store.Id", 1)                  // if 1 it's HWY 401 & Weston.

  object TheCategory extends SessionVar[String](defaultCategory)
  object TheStore extends SessionVar[Store](Store(id=defaultStore))
}
