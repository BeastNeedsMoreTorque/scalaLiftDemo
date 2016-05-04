package code.model

import code.UnitTest
import code.model.GlobalLCBO_IDs.P_KEY
import code.model.prodAdvisor.ProductAdvisorComponentImpl
import net.liftweb.common.Full

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-05-04.
  */
class ProductAdvisorComponentImplTest extends UnitTest {
  class ProductAdvisorComponentImplTest extends ProductAdvisorComponentImpl

  val instance = new ProductAdvisorComponentImplTest
  val drunkShuffler = instance.agent
  object MockProduct extends MetaProduct with ProductRunner with InventoryService {
    // Mock it. Could create (or better yet generate randomly with ScalaCheck) a handful of concrete Product instances.
    override def asyncLoadCache() = {} // intentional Noop here.

    //Need some reasonable simulation for following. With just a bit more work, we could have something really interesting here.
    override def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct]  = IndexedSeq[Product]()
    override def getProductsByCategory(lcboCategory: String) = IndexedSeq[IProduct]()
    override val inventoryByProductIdMap: P_KEY => Option[Inventory] = key => None

  }

  behavior of "No input"
  it should s"advise an empty Stream of products when no products of category can be found" in {
    drunkShuffler.advise(MockProduct, "wine", 5, MockProduct) should equal(Full(Stream()))

  }
}
