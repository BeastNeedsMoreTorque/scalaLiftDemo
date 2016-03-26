package code.snippet

import net.liftweb.http.SessionVar
import net.liftweb.util.Props

/**
  * Created by philippederome on 2015-12-05.
  */
object SessionCache {
  val defaultCategory = Props.get("product.Category", "wine")
  object theCategory extends SessionVar[String](defaultCategory)
  object theRecommendCount extends SessionVar[Int](1)
}
