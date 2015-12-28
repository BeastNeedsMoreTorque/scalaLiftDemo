package code.model

import net.liftweb.util.Props
import net.liftweb.json.JsonParser.parse

/**
  * Created by philippederome on 15-10-26.
  * provides sequence of product categories to display to client, which is configurable via properties, additionally, provides a mapping of queryable pattern-categories to those used as primary categories in LCBO catalog
  */
object LiquorCategory {
  private val catToPrimaryCatMap: Map[String, String] = {
    // we could chain the operations below, but for clarity we spell them out individually
    val productCategoriesMapAsStr = Props.get("product.CategoriesMap", "") // get it in JSON format
    val list = parse(productCategoriesMapAsStr) // contains list of JField(String, JString(String))
    val v = for (elem <- list.children.toVector) yield elem.values // contains vector of (String, String)
    v.map(_.asInstanceOf[(String, String)]).toMap[String, String] // could throw if contents that are configured are in wrong format (violating assumption of pairs (String,String)...
  }
  def toPrimaryCategory(s: String) = catToPrimaryCatMap(s) // maps user selection to a category that is a pattern recognizable by LCBO in queries.

  val toImg: Map[String, String] = {
    // we could chain the operations below, but for clarity we spell them out individually
    val productCategoriesImgMapAsStr = Props.get("product.CategoriesImageMap", "") // get it in JSON format
    val list = parse(productCategoriesImgMapAsStr) // contains list of JField(String, JString(String))
    val v = for (elem <- list.children.toVector) yield elem.values // contains vector of (String, String)
    v.map(_.asInstanceOf[(String, String)]).toMap[String, String] // could throw if contents that are configured are in wrong format (violating assumption of pairs (String,String)...
  }

  val sortedSeq = Props.get("product.Categories", "wine:beer").split(":").toSeq // give wine and beer at a minimum, provides iterable sequence of categories users can select from.
  // The list coincides with LCBO pattern keys for searches by design, but it could be made independent if needed.
}