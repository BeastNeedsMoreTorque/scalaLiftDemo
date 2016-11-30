package code.model

import code.model.GlobalLCBO_IDs._
import code.model.productMocks._

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-08-25.
  */
package object productRunnerMocks {

  object NullInventoryService extends InventoryService {
    override def lcboKey: LCBO_KEY = 1.LcboKeyID
    override val inventoryByProductIdMap: P_KEY => Option[Inventory] = key => None
    override def getProduct(x: LCBO_KEY): Option[Product] = None
    override def getProductKeysByCategory(lcboCategory: String): IndexedSeq[KeyKeeperVals] = IndexedSeq.empty
    override def asyncLoadCache(): Unit = () // intentional Noop/Unit here.
  }
  trait MockProductRunner extends ProductRunner {
    val beers: Seq[MockBeer] = Seq.empty
    val wines: Seq[MockWine] = Seq.empty
    // Could create (or better yet generate randomly with ScalaCheck) a handful of concrete Product instances.
    // Need some reasonable simulation for following. With just a bit more work, we could have something really interesting here.
    override def fetchByStoreCategory(lcboStoreId: LCBO_KEY,
                                      category: String,
                                      requiredSize: Int): ValidatedProducts = Right (
      category match {
        case "beer" => beers.toVector case "wine" => wines.toVector
        case _ => Vector()
      }
    )
  }

  object outOfStockRunner extends MockProductRunner

  object singleBeerRunner extends MockProductRunner {
    override val beers =  List( Heineken)
  }

  object HeinekensBut63Runner extends MockProductRunner {
    override val beers =  Seq.fill(63)( Heineken) ++ Seq(MillStLager) ++ Seq.fill(37)( Heineken)
    override val wines = Seq(OysterBay, ChampagneKrug)
  }

  object HeinekensBut62Runner extends MockProductRunner {
    override val beers =  Seq.fill(62)( Heineken) ++ Seq(MillStLager) ++ Seq.fill(38)( Heineken)
    override val wines = Seq(OysterBay, ChampagneKrug)
  }
}
