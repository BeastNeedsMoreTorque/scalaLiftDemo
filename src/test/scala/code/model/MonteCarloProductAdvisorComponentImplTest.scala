package code.model

import code.UnitTest
import code.model.GlobalLCBO_IDs.{LCBO_KEY, P_KEY}
import code.model.prodAdvisor.MonteCarloProductAdvisorComponentImpl
import code.model.utils.RNG
import scala.collection.IndexedSeq
import cats.data.Xor
import code.model.GlobalLCBO_IDs._

/**
  * Created by philippederome on 2016-05-04.
  */
class MonteCarloProductAdvisorComponentImplTest extends UnitTest {
  class MonteCarloProductAdvisorComponentImplTest extends MonteCarloProductAdvisorComponentImpl

  val emptyProducts = Xor.Right( IndexedSeq[Product]() )
  object MonteCarloInstance extends MonteCarloProductAdvisorComponentImplTest
  val tupsyTurvyClerk = MonteCarloInstance.agent

  object NullInventoryService extends InventoryService {
    override def pKey: P_KEY = 1.PKeyID
    override def lcboKey: LCBO_KEY = 1.LcboKeyID
    override val inventoryByProductIdMap: P_KEY => Option[Inventory] = key => None
    override def getProduct(x: LCBO_KEY): Option[IProduct] = None
    override def getProductKeysByCategory(lcboCategory: String): IndexedSeq[KeyKeeperVals] = IndexedSeq.empty
    override def asyncLoadCache(): Unit = () // intentional Noop/Unit here.
  }

  // Runners
  trait MockProductRunner extends ProductRunner {
    val beers: Seq[MockBeer] = Seq.empty
    val wines: Seq[MockWine] = Seq.empty
    // Could create (or better yet generate randomly with ScalaCheck) a handful of concrete Product instances.
    // Need some reasonable simulation for following. With just a bit more work, we could have something really interesting here.
    override def fetchByStoreCategory(lcboStoreId: LCBO_KEY,
                                      category: String,
                                      requiredSize: Int): ValidatedProducts = Xor.Right (
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
    // depends precisely on  props store.fixedRNGSeed=411
    override val beers =  Seq.fill(63)( Heineken) ++ Seq(MillStLager) ++ Seq.fill(37)( Heineken)
    override val wines = Seq(OysterBay, ChampagneKrug)
  }

  object HeinekensBut62Runner extends MockProductRunner {
    // depends precisely on  props store.fixedRNGSeed=411. Mill St is at a different spot!
    override val beers =  Seq.fill(62)( Heineken) ++ Seq(MillStLager) ++ Seq.fill(38)( Heineken)
    override val wines = Seq(OysterBay, ChampagneKrug)
  }

  // products
  trait MockProduct extends IProduct {
    override def isDiscontinued: Boolean = false
    override def imageThumbUrl: String = "http://lcboapi.com/someimage.png"
    override def streamAttributes: IndexedSeq[AttributeHtmlData] =
      ( AttributeHtmlData("Name:", Name) ::
        AttributeHtmlData("Primary Category:", primaryCategory) ::
        AttributeHtmlData("Price:", price) ::
        AttributeHtmlData("Alcohol content:", alcoholContent) ::
        Nil).filterNot{ attr => attr.value == "null" || attr.value.isEmpty }.toVector
    def identifier: Long
    override def pKey: P_KEY = identifier.PKeyID
    override def lcboKey: LCBO_KEY = identifier.LcboKeyID
  }
  trait MockBeer extends IProduct with MockProduct  {
    override def primaryCategory: String = "beer"
    override def price: String = "$2.00"
    override def alcoholContent: String = "5.0%"
  }

  trait MockWine extends IProduct with MockProduct {
    override def primaryCategory: String = "wine"
    override def price: String = "$15.00"
    override def alcoholContent: String = "16.0%"
  }

  object Heineken extends MockBeer {
    override def identifier: Long = 1
    override def Name: String = "Heineken"
  }

  object MillStLager extends MockBeer {
    override def identifier: Long = 2
    override def alcoholContent: String = "5.5%"
    override def Name: String = "Mill Street Lager"
    override def price: String = "$2.50"
  }

  object OysterBay extends MockWine {
    override def identifier: Long = 1000
    override def alcoholContent: String = "16.0%"
    override def Name: String = "Oyster Bay"
    override def price: String = "$21.00"
  }

  object ChampagneKrug extends MockWine {
    override def identifier: Long = 1001
    override def alcoholContent: String = "14.0%"
    override def Name: String = "Krug Champagne"
    override def price: String = "$150.00"
  }

  behavior of "No product available empty list"
  it should s"advise an empty list of products when using dummy InventoryService and ProductRunner when no products of category can be found" in {
    val rng = RNG.Simple(411)
    tupsyTurvyClerk.advise(rng, NullInventoryService, "wine", 5, outOfStockRunner).map {
      x => x.toList shouldBe empty
    }
  }
  it should s"advise an empty list of products when no products of category can be found" in {
    val categories = Seq("wine", "spirits", "beer", "ciders", "coolers", "non-Alc")
    val rng = RNG.Simple(411)
    val FlagShip = Store
    FlagShip.lcbo_id.set(1) // to make web query good
    categories.foreach(cat => tupsyTurvyClerk.advise(rng, FlagShip, cat, 5, outOfStockRunner).map {
      x => x.toList shouldBe empty
    })
  }

  behavior of "Single product match by category once list of 1 and once empty list"
  it should s"get a Heineken name in first selection for beer when it is the only product" in {
    val rng = RNG.Simple(411)
    tupsyTurvyClerk.advise(rng, NullInventoryService, "beer", 1, singleBeerRunner).map {
      _.headOption.foreach(_._1.Name should equal("Heineken"))
    }
  }
  // following is more to validate previous test. This is not particularly interesting.
  it should s"NOT get a Heineken name in first selection for wine when Heineken is the only (beer) product" in {
    val rng = RNG.Simple(411)
    tupsyTurvyClerk.advise(rng, NullInventoryService, "wine", 1, singleBeerRunner).map {
      case Nil => ; // success: wine != beer, no match.
    }
  }

  // following tests depend on a shuffle of collection indices (post category filter) and normal Scala collection take as selection strategy.
  behavior of "Deterministic random selection of single item among 101 beers: 1 Mill Street among 100 Heinekens fixed seed"
  val rng411 = RNG.Simple(411)
  def validateSelectedName(runner: ProductRunner, rng: RNG, name: String): Unit = {
    tupsyTurvyClerk.advise(rng, NullInventoryService, "beer", 1, runner).map {
      _.headOption.foreach{ _._1.Name should equal(name)}
    }
  }
  // this more primitive test on shuffling indices motivates the next two material examples that do shuffling on products.
  it should s"predict take 1st element from 0 to 100 randomly permuted exactly on specific seed Simple at 63" in {
    val shuffled = RNG.shuffle((0 to 100)).runA(rng411).value
    shuffled.take(1) should equal(Seq(63))
  }
  it should s"get Mill Street Lager or really #63 out of 0 to 100 (64th position in 0-index system, literally 63)" +
  "if seed is set to 411 with 100 Heinekens " +
    s"out of 101!!!" in
    validateSelectedName(HeinekensBut63Runner, rng411, "Mill Street Lager")
  it should s"get Heineken as Mill Street Lager is not in special spot of index value 63 among 101, 100 of which are Heineken!" in
    validateSelectedName(HeinekensBut62Runner, rng411, "Heineken")

}
