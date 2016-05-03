package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-05-02.
  */
class ProductTest {

  object MockProduct extends MetaProduct with ProductFetcher {
    // Mock it
    override def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct]  = IndexedSeq[Product]()
  }

}
