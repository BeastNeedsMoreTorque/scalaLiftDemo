package code.model

import net.liftweb.util.Props
/**
  * Created by philippederome on 15-10-26.
  * Provides sequence of product categories to display to client, which is configurable via config,
  * additionally, provides a mapping of queryable pattern-categories to those used as primary categories in LCBO catalog
  * @param config provides configuration object that gives us the sequence of product categories
  */
class LiquorCategory(config: ConfigPairsRepo) {
  private def getMap(k: String) =
    config.getSeq(k).toMap

  /**
    * @param category a category of products at LCBO such as wine or beer
    * @return a different wording of that category that LCBO uses to specify a primary category
    * for instance LCBO will associate category of "coolers" to the longer primary category name of "Ready-to-Drink/Coolers"
    */
  def toPrimaryCategory(category: String): String = {
    getMap("product.CategoriesMap").
      get(category).fold(category)(identity)
  }

  /**
    * @return a map of product categories to an image file that can be rendered, a web resource
    */
  val toImg = getMap("product.CategoriesImageMap")
  // give wine and beer at a minimum, provides iterable sequence of categories users can select from.
  val categoriesSeq = Props.get("product.Categories", "wine:beer").split(":").toSeq
}
