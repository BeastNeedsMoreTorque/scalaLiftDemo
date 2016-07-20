package code.model

import net.liftweb.util.Props

/**
  * Created by philippederome on 15-10-26.
  * provides sequence of product categories to display to client, which is configurable via properties,
  * additionally, provides a mapping of queryable pattern-categories to those used as primary categories in LCBO catalog
  */
object LiquorCategory {
  private def getMap(k: String) =
    ConfigPairsRepo.configPairsRepoPropsImpl.getSeq(k).toMap

  def toPrimaryCategory(category: String): String = {
    val x = getMap("product.CategoriesMap")
    x.get(category).fold(category)(identity)
  }

  val toImg = getMap("product.CategoriesImageMap")
  val categoriesSeq = Props.get("product.Categories", "wine:beer").split(":").toSeq
  // give wine and beer at a minimum, provides iterable sequence of categories users can select from.
}
