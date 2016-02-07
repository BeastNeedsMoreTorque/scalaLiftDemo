package code.model

import net.liftweb.util.Props
import net.liftweb.json.JsonParser.parse

/**
  * Created by philippederome on 15-10-26.
  * provides sequence of product categories to display to client, which is configurable via properties, additionally, provides a mapping of queryable pattern-categories to those used as primary categories in LCBO catalog
  */
object LiquorCategory {
  private def getMap(propKey: String): Map[String, String] = {
    val json = parse(Props.get(propKey, "") )
    val l = for (elem <- json.children) yield elem.values // contains List of (String, JString(String))
    l.map(_.asInstanceOf[(String, String)]).toMap[String, String] // could throw if contents that are configured are in wrong format (violating assumption of pairs (String,String)...
  }

  val toPrimaryCategory = getMap("product.CategoriesMap")
  val toImg = getMap("product.CategoriesImageMap")
  val categoriesSeq = Props.get("product.Categories", "wine:beer").split(":").toSeq // give wine and beer at a minimum, provides iterable sequence of categories users can select from.
  // The list coincides with LCBO pattern keys for searches by design, but it could be made independent if needed.
}