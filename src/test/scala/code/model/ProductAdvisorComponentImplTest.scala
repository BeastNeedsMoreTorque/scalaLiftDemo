package code.model

import code.UnitTest
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.prodAdvisor.ProductAdvisorComponentImpl
import net.liftweb.common.Full

import scala.collection.IndexedSeq

import Product.fetchByStoreCategory

/**
  * Created by philippederome on 2016-05-04.
  */
class ProductAdvisorComponentImplTest extends UnitTest {
  class ProductAdvisorComponentImplTest extends ProductAdvisorComponentImpl

  val instance = new ProductAdvisorComponentImplTest
  val drunkShuffler = instance.agent


  object MockNoProduct extends ProductRunner with InventoryService {
    // Mock it. Could create (or better yet generate randomly with ScalaCheck) a handful of concrete Product instances.
    override def asyncLoadCache() = {} // intentional Noop here.
    override def pKey: P_KEY = P_KEY(1)
    override def lcboId: LCBO_ID = LCBO_ID(1)
    //Need some reasonable simulation for following. With just a bit more work, we could have something really interesting here.
    override def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct] = IndexedSeq[Product]()
    override def getProductsByCategory(lcboCategory: String) = IndexedSeq[IProduct]()
    override val inventoryByProductIdMap: P_KEY => Option[Inventory] = key => None
  }

  behavior of "No input"
  it should s"advise an empty Stream of products when no products of category can be found" in {
    drunkShuffler.advise(MockNoProduct, "wine", 5, MockNoProduct) should equal(Full(Stream()))
  }
}
