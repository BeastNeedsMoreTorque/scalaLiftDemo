package code.model

import code.UnitTest
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.prodAdvisor.ProductAdvisorComponentImpl
import net.liftweb.common.Full
import scala.collection.IndexedSeq

// Highly experimental (scalacheck)
import org.scalacheck.Arbitrary._
import org.scalacheck.Prop._
import org.scalacheck.Gen


/**
  * Created by philippederome on 2016-05-04.
  */
class ProductAdvisorComponentImplTest extends UnitTest {
  class ProductAdvisorComponentImplTest extends ProductAdvisorComponentImpl

  val instance = new ProductAdvisorComponentImplTest
  val drunkShuffler = instance.agent

  val MockInventoryService = new InventoryService {
    override def pKey: P_KEY = P_KEY(1)
    override def lcboId: LCBO_ID = LCBO_ID(1)
    override val inventoryByProductIdMap: P_KEY => Option[Inventory] = key => None
    override def getProductsByCategory(lcboCategory: String) = IndexedSeq[IProduct]()
    override def asyncLoadCache() = {} // intentional Noop here.
  }


  behavior of "No input"
  val outOfStockRunner = new ProductRunner {
    override def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct] = IndexedSeq[Product]()
  }
  it should s"advise an empty list of products when using dummy InventoryService and ProductRunner when no products of category can be found" in {
    drunkShuffler.advise(MockInventoryService, "wine", 5, outOfStockRunner) match {
      case Full(x) => x.toList shouldBe empty
      case _ => 1 should equal(2) // we should never get here
    }
  }

  it should s"advise an empty list of products when no products of category can be found" in {
    val categories = Seq("wine", "spirits", "beer", "ciders", "coolers", "non-Alc")
    categories.foreach(cat => drunkShuffler.advise(Store, cat, 5, outOfStockRunner) match {
      case Full(x) => x.toList shouldBe empty
      case _ => true should equal(false) // we should never get here
    })
  }


  //val ints = Gen.choose(1, 1000), eventually might use that.
  object TestProduct extends IProduct {
    override def pKey: P_KEY = P_KEY(1)
    override def lcboId: LCBO_ID = LCBO_ID(1)
    override def Name: String = "Heineken"
    override def primaryCategory: String = "beer"
    override def isDiscontinued: Boolean = false
    override def totalPackageUnits: Int = 1
    override def imageThumbUrl: String = "http://www.devnull.com"
    override def Package: String = "bottle"

    // Change unit of currency from cents to dollars and Int to String
    override def price: String = "$2.00"
    override def streamAttributes: IndexedSeq[Attribute] = Product.streamAttributes
    override def getCachedItem: (IProduct) => Option[IProduct] = s => Some(s) // identity, kind of.
  }

  val mockProductRunner = new ProductRunner {
    // Could create (or better yet generate randomly with ScalaCheck) a handful of concrete Product instances.
    //Need some reasonable simulation for following. With just a bit more work, we could have something really interesting here.
    override def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct] = Vector(TestProduct)
  }
  it should s"get a Heineken name in first selection for beer" in {
    drunkShuffler.advise(MockInventoryService, "beer", 1, mockProductRunner) match {
      case Full(x) =>
        x.headOption.map(_._1.Name should equal("Heineken"))
      case _ =>  1 should equal(2) // we don't care about this.
    }
  }

}
