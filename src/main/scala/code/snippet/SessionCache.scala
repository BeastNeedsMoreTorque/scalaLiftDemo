package code.snippet

import net.liftweb.http.SessionVar
import net.liftweb.util.Props

/**
  * Created by philippederome on 2015-12-05.
  * In Lift, it's sensible to keep state on server side like this, because the different parts of a page (snippets) don't share data
  * and it would probably be messy to aggregate these values in JS and have it been pushed with Ajax calls during user product interaction.
  */
object SessionCache {
  val defaultCategory = Props.get("product.Category", "wine")
  object theCategory extends SessionVar[String](defaultCategory)
  object theRecommendCount extends SessionVar[Int](1)
}
