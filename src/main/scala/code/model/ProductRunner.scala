package code.model

import code.model.GlobalLCBO_IDs.LCBO_KEY
import scala.collection.IndexedSeq
import cats.data.Xor

/**
  * Created by philippederome on 2016-05-03.
  * Picture a staff running to fetch the products within the implied context of a store.
  */
trait ProductRunner {
  /**
    * captures first error in obtaining a sequence of products from sources with errors or the products themselves
    */
  type ValidatedProducts = Xor[Throwable, IndexedSeq[IProduct]]
  /**
    * fetches available products within a store for a given category for a required size
    * @param lcboStoreId identifier of the store as known to the LCBO.
    * @param category category of products requested
    * @param requiredSize max amount of items requested
    * @return available products matching selection criteria possibly fewer items than requiredSize
    */
  def fetchByStoreCategory(lcboStoreId: LCBO_KEY, category: String, requiredSize: Int): ValidatedProducts
}
