package code.model

import net.liftweb.util.Props
import net.liftweb.json.JsonParser.parse
import scala.collection.breakOut

/**
  * Created by philippederome on 15-10-26.
  * provides sequence of product categories to display to client, which is configurable via properties, additionally, provides a mapping of queryable pattern-categories to those used as primary categories in LCBO catalog
  */
object LiquorCategory extends propsSeqReader {
  private def getMap(propKey: String): Map[String, String] =
    getSeq(propKey).toMap

  val toPrimaryCategory = getMap("product.CategoriesMap")
  val toImg = getMap("product.CategoriesImageMap")
  val categoriesSeq = Props.get("product.Categories", "wine:beer").split(":").toSeq // give wine and beer at a minimum, provides iterable sequence of categories users can select from.
}