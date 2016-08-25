package code.model

import code.UnitTest
import code.model.prodAdvisor.{MonteCarloProductAdvisorComponentImpl, ProductAdvisorDispatcher}
import code.model.utils.RNG
import code.model.ProductRunnerMocks._

/**
  * Created by philippederome on 2016-05-04.
  */
class MonteCarloProductAdvisorComponentImplTest extends UnitTest {
  class MonteCarloProductAdvisorComponentImplTest extends MonteCarloProductAdvisorComponentImpl

  object TupsyTurvy extends ProductAdvisorDispatcher with MonteCarloProductAdvisorComponentImpl

  implicit val rng411 = RNG.Simple(411)

  behavior of "No product available empty list"
  it should s"advise an empty list of products when using dummy InventoryService and ProductRunner when no products of category can be found" in {
    TupsyTurvy.advise(NullInventoryService, "wine", 5, outOfStockRunner).map {
      x => x.toList shouldBe empty
    }
  }
  it should s"advise an empty list of products when no products of category can be found" in {
    val categories = Seq("wine", "spirits", "beer", "ciders", "coolers", "non-Alc")
    val FlagShip = Store
    FlagShip.lcbo_id.set(1) // to make web query good
    categories.foreach(cat => TupsyTurvy.advise(FlagShip, cat, 5, outOfStockRunner).map {
      x => x.toList shouldBe empty
    })
  }

  behavior of "Single product match by category once list of 1 and once empty list"
  it should s"get a Heineken name in first selection for beer when it is the only product" in {
    TupsyTurvy.advise(NullInventoryService, "beer", 1, singleBeerRunner).map {
      _.headOption.foreach(_._1.Name should equal("Heineken"))
    }
  }
  // following is more to validate previous test. This is not particularly interesting.
  it should s"NOT get a Heineken name in first selection for wine when Heineken is the only (beer) product" in {
    TupsyTurvy.advise(NullInventoryService, "wine", 1, singleBeerRunner).map {
      case Nil => ; // success: wine != beer, no match.
    }
  }

  // following tests depend on a shuffle of collection indices (post category filter) and normal Scala collection take as selection strategy.
  behavior of "Deterministic random selection of single item among 101 beers: 1 Mill Street among 100 Heinekens fixed seed"
  def validateSelectedName(runner: ProductRunner, rng: RNG, name: String): Unit = {
    TupsyTurvy.advise(NullInventoryService, "beer", 1, runner).map {
      _.headOption.foreach{ _._1.Name should equal(name)}
    }
  }
  // this more primitive test on shuffling indices motivates the next two material examples that do shuffling on products.
  it should s"predict take 1st element from 0 to 100 randomly permuted exactly on specific seed Simple at 63" in {
    val shuffled = RNG.shuffle((0 to 100)).runA(rng411).value
    shuffled.take(1) should equal(Seq(63))
  }

  // depends precisely on position of Mill Street Lager in runner and rng411
  it should s"get Mill Street Lager or really #63 out of 0 to 100 (64th position in 0-index system, literally 63)" +
  "if seed is set to 411 with 100 Heinekens " +
    s"out of 101!!!" in
    validateSelectedName(HeinekensBut63Runner, rng411, "Mill Street Lager")
  // depends precisely on position of Mill Street Lager in runner and rng411
  it should s"get Heineken as Mill Street Lager is not in special spot of index value 63 among 101, 100 of which are Heineken!" in
    validateSelectedName(HeinekensBut62Runner, rng411, "Heineken")

}
