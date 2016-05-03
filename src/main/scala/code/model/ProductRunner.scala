package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-05-03.
  */
trait ProductRunner { // Picture a staff running to fetch the products within the implied context of a store.
  def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct]
}
