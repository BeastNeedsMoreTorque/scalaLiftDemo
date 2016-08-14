package code.model.prodAdvisor

import code.model.utils.RNG
import code.model._
import net.liftweb.util.Props
import cats.data.Xor
import scala.collection.{IndexedSeq, Iterable, Seq}

/**
  * Created by philippederome on 2016-05-02. Cake Pattern style.
  * Note that objects of type RNG are always called on methods that return objects and the meaningful
  * return a new RNG (pattern state transformations, apparently common in Scalaz). This is inspired by Functional Programming in Scala Chapter 6.
  */
trait ProductAdvisorComponent {
  /**
    * Selection is an iterable of product along with a count of many there are
    */
  type Selection = Iterable[(IProduct, Long)]
  /**
    * captures exceptions as errors in Xor if any, otherwise a selection
    */
  type ValidateSelection = Xor[Throwable, Selection]

  /**
    * @return a ProductAvisor who can provide advice on products
    */
  def agent: ProductAdvisor

  /**
    * An interface to provide advice (recommendation) for LCBO products modelled by a simple single method.
    */
  trait ProductAdvisor {
    /**
      * @param rng a Random Number Generator state item.
      * @param invService an entity capable of determining number of items for each product that can be sold
      * @param category category of products to advise on (this is deliberately coarse to simplify application)
      * @param requestSize the desired request size for the advice, which need not be fulfilled but should be on best efforts basis
      * @param runner a service agent that can select products of a given category in a store; think of the agent
      *               running around a counter for which clients have no access, in the back office.
      * @return ValidateSelection matching the request input parameters
      */
    def advise(rng: RNG,
               invService: InventoryService,
               category: String,
               requestSize: Int,
               runner: ProductRunner): ValidateSelection
  }
}

/**
  * implements ProductAdvisor delegating to a polymorphic intermediary (agent) that actually provides the service.
  */
trait ProductAdvisorDispatcher {
  this: ProductAdvisorComponent =>

  /**
    *
    * @param rng a Random Number Generator state item.
    * @param invService an entity capable of determining number of items for each product that can be sold
    * @param category category of products to advise on (this is deliberately coarse to simplify application)
    * @param requestSize the desired request size for the advice, which need not be fulfilled but should be on best efforts basis
    * @param runner a service agent that can select products of a given category in a store; think of the agent
    *               running around a counter for which clients have no access, in the back office.
    * @return ValidateSelection
    */
  def advise(rng: RNG,
             invService: InventoryService,
             category: String,
             requestSize: Int,
             runner: ProductRunner): ValidateSelection =
    agent.advise(rng, invService, category, requestSize, runner)
}

/**
  * the suggested implementation below is for fun. A more realistic/commercial one would use proper analytics instead.
  */
trait MonteCarloProductAdvisorComponentImpl extends ProductAdvisorComponent {
  /**
    * @return a ProductAdvisor object, which selects products randomly without consideration about user preferences.
    */
  def agent: ProductAdvisor = {
    // manual Dependency Injection,specifying dependencies as parameters to MonteCarloAdvisor
    val liquorCategory = LiquorCategory(ConfigPairsRepo.configPairsRepoPropsImpl)
    val maxSampleSize = Props.getInt("advisor.maxSampleSize", 0)
    new MonteCarloAdvisor(liquorCategory, maxSampleSize)
  }

  /**
    * beware: this advisor could be hammered! To do: Find a more practical, corporate alternative instead with some ML analytics.
    * @param liquorCategory has info/context about LCBO categories
    * @param maxSampleSize the maximum number of product items to request synchronously from LCBO, an excessively high value
    *                      will compromise usability since thisis synchronous.
    */
  class MonteCarloAdvisor(liquorCategory: LiquorCategory,
                          maxSampleSize: Int) extends ProductAdvisor {

    /**
      * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
      * Select a random product that matches the parameters subject to a max sample size.
      * @param rng a Random Number Generator state item.
      * @param invService  a InventoryService trait that has a few abstract methods about inventory in stock or
      *                      ability to sync cache from LCBO Web API (theoretically inventory update or provisioning).
      * @param category    a String such as beer, wine, mostly matching primary_category at LCBO,
      *                    or an asset category (for query only not to compare results and filter!).
      * @param requestSize a number representing how many items we need to propose as recommendation
      * @param runner a ProductRunner, responsible to obtain products in a store for a given vategory.
      * @return ValidateSelection
      */
    def advise(rng: RNG,
               invService: InventoryService,
               category: String,
               requestSize: Int,
               runner: ProductRunner): ValidateSelection = {
      val lcboProdCategory = liquorCategory.toPrimaryCategory(category)
      // the shuffling in getShuffledProducts is predetermined by rng (and not other class calls to random generation routines),
      // and when cache fails, it is predetermined by the actual contents we get from LCBO via getSerialResult.
      getShuffledProducts(invService, runner, rng, invService.getProductKeysByCategory(lcboProdCategory),
        category, lcboProdCategory, requestSize)
    }

    /**
      * Note well: this would be a PURE FUNCTION, despite that caller uses randomization and makes use of cached data.
      * Randomization can be made deterministic by configuration (UseRandomSeed being false).
      * Caching can be a no-op by providing an implementation of invService that does not cache
      * and has no side effect (by default it caches).
      * So this is "pure" relative to implementations of InventoryService and ProductRunner making guarantees to being pure on the methods we use here.
      * That guarantee could be provided in a unit test environment.
      * @param invService an inventory service instance that allows us to obtain inventory info for products in a store
      * @param runner a ProductRunner instance
      * @param rng a Random Number Generator state item.
      * @param initialProductKeys a tentative collection of products that would satisfy user request. If it does, we random sample from it,
      *                        otherwise we go to LCBO to get some fresh ones synchronously and shuffle them.
      * @param category the category of the product
      * @param lcboProdCategory specifies the expected value of primary_category on feedback from LCBO.
      *                         It's been known that they would send a Wiser's Whiskey on a wine request.
      * @param requestSize amount of items the client is requesting for a recommendation/advice
      * @return ValidateSelection.
      */
    private def getShuffledProducts(invService: InventoryService,
                            runner: ProductRunner,
                            rng: RNG,
                            initialProductKeys: IndexedSeq[KeyKeeperVals],
                            category: String,
                            lcboProdCategory: String,
                            requestSize: Int): ValidateSelection = {
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

        if (cachedIds.nonEmpty) Xor.Right(cachedIds.map(inStockItems))
        // when nonEmpty, get back the items that the ids have been selected (we don't stream because we know inventory > 0)
        else getSerialResult(invService, runner, requestSize, category, lcboProdCategory, rr)
    }

    /**
      * @param invService an inventory service instance
      * @param runner a ProductRunner instance
      * @param requestSize amount of items the client is requesting for a recommendation/advice
      * @param category the category of the product
      * @param lcboProdCategory specifies the expected value of primary_category on feedback from LCBO.
      *                         It's been known that they would send a Wiser's Whiskey on a wine request.
      * @param r a Random Number Generator state item.
      * @return ValidateSelection (since there's web client dependency, need to capture exceptions low level exceptions here).
      */
    private def getSerialResult(invService: InventoryService,
                        runner: ProductRunner,
                        requestSize: Int,
                        category: String,
                        lcboProdCategory: String,
                        r: RNG): ValidateSelection =
    for {
      prods <- runner.fetchByStoreCategory(invService.lcboId, category, maxSampleSize)
      // take a hit of one go to LCBO, querying by category, no more.
      permutedIndices <- Xor.Right(RNG.shuffle(prods.indices).runA(r).value)
      // stream avoids checking primary category on full collection (the permutation is done though).
      stream <- Xor.right(for {id <- permutedIndices.toStream
                    p = prods(id) if p.primaryCategory == lcboProdCategory} yield p)
      res <- Xor.Right(stream.take(requestSize).zip(Seq.fill(requestSize)(0.toLong)))
    // filter by category before take as LCBO does naive (or generic) pattern matching on all fields
    // and then zip with list of zeroes because we are too slow to obtain inventories.
    } yield res
  }
}
