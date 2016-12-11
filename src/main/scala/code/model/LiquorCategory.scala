package code.model

import net.liftweb.util.Props
/**
  * Created by philippederome on 15-10-26.
  * Provides sequence of product categories to display to client, which is configurable via config,
  * additionally, provides a mapping of queryable pattern-categories to those used as primary categories in LCBO catalog
  */
class LiquorCategory {
  /**
    * @return a map of product categories to an image file that can be rendered, a web resource
    */
  val toImg = getMap("product.CategoriesImageMap")
  // give wine and beer at a minimum, provides iterable sequence of categories users can select from.
  val categories = Props.get("product.Categories", "wine:beer").split(":")

  /**
    * @param category a category of products at LCBO such as wine or beer
    * @return a different wording of that category that LCBO uses to specify a primary category
    * for instance LCBO will associate category of "coolers" to the longer primary category name of "Ready-to-Drink/Coolers"
    */
  def toPrimaryCategory(category: String): String =
    getMap("product.CategoriesMap").
      get(category).
      fold(category)(identity)

  private def getMap(k: String) =
    ConfigPairsRepo.getSeq(k).toMap
}
