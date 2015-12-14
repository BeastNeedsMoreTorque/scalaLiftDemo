package code.snippet

import net.liftweb.common.{Full, Box}
import net.liftweb.http.SessionVar
import net.liftweb.util.Props
import net.liftweb.util.Helpers._

/**
  * Created by philippederome on 2015-12-05.
  */
object SessionCache {
  private val defaultCategory = Props.get("product.Category", "wine")
  // None defined is acceptable though not great.
  private val defaultStore = Props.getInt("store.Id", 1)
  private val defaultLatitude: String = Props.get("user.latitude", "")
  private val defaultLongitude: String = Props.get("user.longitude", "")

  // if 1 it's HWY 401 & Weston.

  object TheCategory extends SessionVar[String](defaultCategory)

  object TheStore extends SessionVar[Int](defaultStore)

  object TheLatitude extends SessionVar[String](defaultLatitude)
  object TheLongitude extends SessionVar[String](defaultLongitude)

}
