package code.model
import code.UnitTest
import code.model.prodAdvisor.{ProductAdvisorDispatcher, SlowAdvisorComponentImpl}
import code.model.productRunnerMocks._
import cats.implicits._

/**
  * Created by philippederome on 2016-08-25.
  */
class SlowAdvisorComponentImplTest extends UnitTest {
  class SlowAdvisorComponentImplTest extends SlowAdvisorComponentImpl

  object Turtle extends ProductAdvisorDispatcher with SlowAdvisorComponentImpl

  implicit val rn = ProductAdvisorDispatcher.defaultRNG
  behavior of "No product available empty list"
  it should s"advise an empty list of products when using dummy InventoryService and ProductRunner when no products of category can be found" in {
    Turtle.advise(NullInventoryService, "wine", 5, outOfStockRunner).map {
      x => x.toList shouldBe empty
    }
  }
  it should s"advise an empty list of products when no products of category can be found" in {
    val categories = Seq("wine", "spirits", "beer", "ciders", "coolers", "non-Alc")
    val FlagShip = Store
    FlagShip.lcbo_id.set(1) // to make web query good
    categories.foreach(cat => Turtle.advise(FlagShip, cat, 5, outOfStockRunner).map {
      x => x.toList shouldBe empty
    })
  }

  behavior of "Single product match by category once list of 1 and once empty list"
  it should s"get a Heineken name in first selection for beer when it is the only product" in {
    Turtle.advise(NullInventoryService, "beer", 1, singleBeerRunner).map {
      _.headOption.foreach(_._1.Name should equal("Heineken"))
    }
  }
  // following is more to validate previous test. This is not particularly interesting.
  it should s"NOT get a Heineken name in first selection for wine when Heineken is the only (beer) product" in {
    Turtle.advise(NullInventoryService, "wine", 1, singleBeerRunner).map {
      case Nil => ; // success: wine != beer, no match.
    }
  }

  // following tests depend on normal Scala collection take as selection strategy.
  behavior of "Selection of single item among 101 beers: 1 Mill Street among 100 Heinekens"
  def validateSelectedName(runner: ProductRunner, index: Int, name: String): Unit = {
    val beers = Turtle.advise(NullInventoryService, "beer", 101, runner).map {
      x: SlowAdvisorComponentImplTest.this.Turtle.Selection =>
      val y = x.toVector
      if (y.size > index) y(index)._1.Name should equal(name)
    }
  }

  // in good part to contrast with more complex MonteCarlo advisor.
  it should """get Mill Street Lager or really #63 out of 0 to 100 (64th position in 0-index system, literally 63)
    |for Runner63 with 100 Heinekens out of 101!!!""" in
    validateSelectedName(HeinekensBut63Runner, 63, "Mill Street Lager")
  it should """get Mill Street Lager or really #63 out of 0 to 100 (64th position in 0-index system, literally 63)
     |for Runner63 with 100 Heinekens out of 101!!!""" in
    validateSelectedName(HeinekensBut62Runner, 62, "Mill Street Lager")

}
