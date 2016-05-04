package code.model

import code.UnitTest
import code.model.prodAdvisor.ProductAdvisorComponentImpl

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-05-04.
  */
class ProductAdvisorComponentImplTest extends UnitTest {
  class ProductAdvisorComponentImplTest extends ProductAdvisorComponentImpl

  val instance = new ProductAdvisorComponentImplTest
  // now what?!? How do we test well hidden MonteCarloAdvisor?
  object MockProduct extends MetaProduct with ProductRunner {
    // Mock it. Could create (or better yet generate randomly with ScalaCheck) a handful of concrete Product instances.
    override def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct]  = IndexedSeq[Product]()
  }
}
