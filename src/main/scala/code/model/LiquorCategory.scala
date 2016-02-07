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
    l.map(_.asInstanceOf[(String, String)]).toMap // could throw if contents that are configured are in wrong format (violating assumption of pairs (String,String)...
    // Compiler cannot know I am putting Strings in the JSON data, so trust me with the dirty cast.
    // @see http://ochafik.com/blog/?p=393 interesting alternative but would yield type erasure warning in this case:
    // elements collect { case se: SpecialElement if accept(se) => transform(se) } would become (l.collect { case x: (String, String) => x }).toMap
  }

  val toPrimaryCategory = getMap("product.CategoriesMap")
  val toImg = getMap("product.CategoriesImageMap")
  val categoriesSeq = Props.get("product.Categories", "wine:beer").split(":").toSeq // give wine and beer at a minimum, provides iterable sequence of categories users can select from.
}