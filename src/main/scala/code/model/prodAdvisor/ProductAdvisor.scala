package code.model.prodAdvisor

import code.model.utils.RNG
import code.model._
import net.liftweb.util.Props
import cats.data.Xor
import scala.util.Try
import scala.collection.{IndexedSeq, Iterable, Seq}

/**
  * Created by philippederome on 2016-05-02. Cake Pattern style.
  * Note that objects of type RNG are always called on methods that return objects and the meaningful
  * return a new RNG (pattern state transformations, apparently common in Scalaz). This is inspired by Functional Programming in Scala Chapter 6.
  * Abstracted out in May from Store so that pieces of functionality can be testable from a unit perspective.
  * First cut of MonteCarloAdvisor is indeed almost testable! In its original form, it was impossible to test well with side effects and concrete
  * classes standing in the way.
  */
trait ProductAdvisorComponent {
  def agent: ProductAdvisor

  trait ProductAdvisor {
    def advise(rng: RNG,
               invService: InventoryService,
               category: String,
               requestSize: Int,
               runner: ProductRunner): Xor[Throwable, Iterable[(IProduct, Long)]]
  }

}

trait ProductAdvisorDispatcher   {
  this: ProductAdvisorComponent =>

  def advise(rng: RNG,
             invService: InventoryService,
             category: String,
             requestSize: Int,
             runner: ProductRunner): Xor[Throwable, Iterable[(IProduct, Long)]] =
    agent.advise(rng, invService, category, requestSize, runner)
}

// the suggested implementation below is for fun. A more realistic/commercial one would use proper analytics instead.
trait ProductAdvisorComponentImpl extends ProductAdvisorComponent {
  def agent: ProductAdvisor = {
    // manual Dependency Injection,specifying dependencies as parameters to MonteCarloAdvisor
    val liquorCategory = LiquorCategory(ConfigPairsRepo.configPairsRepoPropsImpl)
    val maxSampleSize = Props.getInt("advisor.maxSampleSize", 0)
    new MonteCarloAdvisor(liquorCategory, maxSampleSize)
  }

  // beware: this advisor could be hammered! To do: Find a more practical, corporate alternative instead.
  class MonteCarloAdvisor(liquorCategory: LiquorCategory,
                          maxSampleSize: Int) extends ProductAdvisor {

    /**
      * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
      * Select a random product that matches the parameters subject to a max sample size.
      *
      * @param invService    a InventoryService trait that has a few abstract methods about inventory in stock or
      *                      ability to sync cache from LCBO Web API (theoretically inventory update or provisioning).
      * @param category    a String such as beer, wine, mostly matching primary_category at LCBO,
      *                    or an asset category (for query only not to compare results and filter!).
      * @param requestSize a number representing how many items we need to propose as recommendation
      * @param runner a ProductRunner, responsible to obtain products in a store for a given vategory.
      * @return quantity found in inventory for product and the product
      */
    def advise(rng: RNG,
               invService: InventoryService,
               category: String,
               requestSize: Int,
               runner: ProductRunner): Xor[Throwable, Iterable[(IProduct, Long)]] = {

      val lcboProdCategory = liquorCategory.toPrimaryCategory(category)
      // the shuffling in getShuffledProducts is predetermined by rng (and not other class calls to random generation routines),
      // and when cache fails, it is predetermined by the actual contents we get from LCBO via getSerialResult.
      getShuffledProducts(invService, runner, rng, invService.getProductKeysByCategory(lcboProdCategory), category, lcboProdCategory, requestSize)
    }

    /**
      * Note well: this would be a PURE FUNCTION, despite that caller uses randomization and makes use of cached data.
      * Randomization can be made deterministic by configuration (UseRandomSeed being false).
      * Caching can be a no-op by providing an implementation of invService that does not cache
      * and has no side effect (by default it caches).
      * So this is "pure" relative to implementations of InventoryService and ProductRunner making guarantees to being pure on the methods we use here.
      * That guarantee could be provided in a unit test environment.
      * @param invService an inventory service instance
      * @param runner a ProductRunner instance
      * @param rng a Random Number Generator state item.
      * @param initialProducts a tentative collection of products that would satisfy user request. If it does, we random sample from it,
      *                        otherwise we go to LCBO to get some fresh ones synchronously and shuffle them.
      * @param category the category of the product
      * @param lcboProdCategory specifies the expected value of primary_category on feedback from LCBO.
      *                         It's been known that they would send a Wiser's Whiskey on a wine request.
      * @param requestSize amount of items the client is requesting for a recommendation/advice
      * @return Xor[Throwable, Iterable[(IProduct, Long)]]  captures exceptions as errors in Xor if any, otherwise an Iterable of IProducts with their
      *         quantities.
      */
    private def getShuffledProducts(invService: InventoryService,
                            runner: ProductRunner,
                            rng: RNG,
                            initialProductKeys: IndexedSeq[KeyKeeperVals],
                            category: String,
                            lcboProdCategory: String,
                            requestSize: Int): Xor[Throwable, Iterable[(IProduct, Long)]] = {
      val prods = {
        val inStockItems = {
          for {p <- initialProductKeys
               inv <- invService.inventoryByProductIdMap(p.pKey)
               q = inv.quantity if q > 0
               prod <- invService.getProduct(p.lcboId)} yield (prod, q)
        }
        // products are loaded before inventories (when loaded asynchronously) and we might have no inventory, hence we test for positive quantity.

        invService.asyncLoadCache() // if we never loaded the cache, do it. Note: useful even if we have product of matching inventory
        // to find out up to date inventory
        // Ideally this asyncLoadCache could be a metaphor for a just in time restocking request given that our cache could be empty with cache representing
        // "real inventory".

        val (rr, cachedIds) = RNG.collectSample(inStockItems.indices, requestSize).run(rng).value
        // shuffle only on the indices not full items (easier on memory mgmt).

        if (cachedIds.nonEmpty) Try {cachedIds.map(inStockItems)}
        // when nonEmpty, get back the items that the ids have been selected (we don't stream because we know inventory > 0)
        else getSerialResult(invService, runner, requestSize, category, lcboProdCategory, rr)
      }
      Xor.fromTry(prods)
    }

    /**
      * @param invService an inventory service instance
      * @param runner a ProductRunner instance
      * @param requestSize amount of items the client is requesting for a recommendation/advice
      * @param category the category of the product
      * @param lcboProdCategory specifies the expected value of primary_category on feedback from LCBO.
      *                         It's been known that they would send a Wiser's Whiskey on a wine request.
      * @param r a Random Number Generator state item.
      * @return 0 quantity found in inventory for product (unknown to be resolved in JS) and the product
      */
    private def getSerialResult(invService: InventoryService,
                        runner: ProductRunner,
                        requestSize: Int,
                        category: String,
                        lcboProdCategory: String,
                        r: RNG): Try[Iterable[(IProduct, Long)]] =
    for {
      prods <- runner.fetchByStoreCategory(invService.lcboId, category, maxSampleSize)
      // take a hit of one go to LCBO, querying by category, no more.
      permutedIndices = RNG.shuffle(prods.indices).runA(r).value
      // stream avoids checking primary category on full collection (the permutation is done though).
      stream = for {id <- permutedIndices.toStream
                    p = prods(id) if p.primaryCategory == lcboProdCategory} yield p
      res = stream.take(requestSize).zip(Seq.fill(requestSize)(0.toLong))
    // filter by category before take as LCBO does naive (or generic) pattern matching on all fields
    // and then zip with list of zeroes because we are too slow to obtain inventories.
    } yield res
  }
}
