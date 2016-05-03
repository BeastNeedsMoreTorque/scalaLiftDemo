package code.model.prodAdvisor

import code.model.utils.RNG
import code.model.{IProduct, IStore, LiquorCategory, ProductFetcher}
import net.liftweb.common.Box
import net.liftweb.util.Helpers._
import net.liftweb.util.Props

import scala.collection.{IndexedSeq, Iterable, Seq}
import scala.util.Random

/**
  * Created by philippederome on 2016-05-02. Abstracted out in May from Store so that pieces of functionality can be testable from a unit perspective.
  * First cut of ShufflingProductRecommender is indeed almost testable! In its original form, it was impossible to test well with side effects and concrete
  * classes standing in the way.
  */
trait ProductAdvisorComponent {
  def agent: ProductAdvisor

  trait ProductAdvisor {
    def recommend(theStore: IStore,
                  category: String,
                  requestSize: Int,
                  runner: ProductFetcher): Box[Iterable[(IProduct, Long)]]
  }

}

trait ProductAdvisorDispatcher   {
  this: ProductAdvisorComponent =>

  def recommend(theStore: IStore,
                category: String,
                requestSize: Int,
                runner: ProductFetcher): Box[Iterable[(IProduct, Long)]] =
    agent.recommend(theStore, category, requestSize, runner)
}

// the suggested implementation below is for fun. A more realistic/commercial one would use proper analytics instead.
trait ShufflingProductRecommender extends ProductAdvisorComponent {
  def agent = new ProductAdvisorImpl

  class ProductAdvisorImpl  extends ProductAdvisor {

    object Shuffler  {
      val UseRandomSeed = Props.getBool("store.useRandomSeed", true)
      val FixedRNGSeed = Props.getInt("store.fixedRNGSeed", 21)
      val MaxSampleSize = Props.getInt("store.maxSampleSize", 0)
    }

    /**
      * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
      * Select a random product that matches the parameters subject to a max sample size.
      *
      * @param category    a String such as beer, wine, mostly matching primary_category at LCBO, or an asset category (for query only not to compare results and filter!).
      * @param requestSize a number representing how many items we need to sample
      * @return quantity found in inventory for product and the product
      */
    def recommend(theStore: IStore,
                           category: String,
                           requestSize: Int,
                           fetcher: ProductFetcher): Box[Iterable[(IProduct, Long)]] = {

      def UseRandomSeed: Boolean = Shuffler.UseRandomSeed

      def FixedRNGSeed: Int = Shuffler.FixedRNGSeed

      /**
        * @param lcboProdCategory specifies the expected value of primary_category on feedback from LCBO. It's been known that they would send a Wiser's Whiskey on a wine request.
        * @return 0 quantity found in inventory for product (unknown to be resolved in JS) and the product
        */
      def getSerialResult(theStore: IStore,
                          fetcher: ProductFetcher,
                          requestSize: Int,
                          category: String,
                          lcboProdCategory: String, r: RNG) = {

        val prods = fetcher.fetchByStoreCategory(theStore.lcboId, category, Shuffler.MaxSampleSize) // take a hit of one go to LCBO, querying by category, no more.
        val (permutedIndices, rr) = RNG.shuffle(prods.indices).run(r)
        // stream avoids checking primary category on full collection (the permutation is done though).
        val stream = for (id <- permutedIndices.toStream;
                          p = prods(id) if p.primaryCategory == lcboProdCategory) yield p
        stream.take(requestSize).zip(Seq.fill(requestSize)(0.toLong)) // filter by category before take as LCBO does naive (or generic) pattern matching on all fields
        // and then zip with list of zeroes because we are too slow to obtain inventories.
      }

      // Note well: this would be a PURE FUNCTION, despite that caller uses randomization and makes use of cached data.
      // Randomization can be made deterministic by configuration (UseRandomSeed being false). Caching can be a no-op by providing an implementation of IStore that does not cache
      // and has no side effect (by default it caches).
      // So this is "pure" relative to implementations of IStore and ProductFetcher making guarantees to being pure on the methods we use here.
      // That guarantee can be provided in a unit test environment.
      def getShuffledProducts(theStore: IStore,
                              fetcher: ProductFetcher,
                              rngSeed: RNG,
                              initialProducts: IndexedSeq[IProduct],
                              category: String,
                              lcboProdCategory: String,
                              requestSize: Int) = tryo {
        val inStockItems = {
          for (p <- initialProducts;
               inv <- theStore.inventoryByProductIdMap(p.pKey);
               q = inv.quantity if q > 0) yield (p, q)
        }
        // products are loaded before inventories and we might have none
        theStore.asyncLoadCache() // if we never loaded the cache, do it (fast lock free test). Note: useful even if we have product of matching inventory
        val (cachedIds, rr) = RNG.collectSample(inStockItems.indices, requestSize).run(rngSeed) // shuffle only on the indices not full items (easier on memory mgmt).
        if (cachedIds.nonEmpty) cachedIds.map(inStockItems) // get back the items that the ids have been selected (we don't stream because we know inventory > 0)
        else getSerialResult(theStore, fetcher, requestSize, category, lcboProdCategory, rr)
      }

      val lcboProdCategory = LiquorCategory.toPrimaryCategory(category)
      // the shuffling in getShuffledProducts is predetermined by rngSeed (and not other class calls to random generation routines),
      // and when cache fails, it is predetermined by the actual contents we get from LCBO via getSerialResult.
      val rng = RNG.Simple(if (UseRandomSeed) Random.nextInt() else FixedRNGSeed)

      getShuffledProducts(theStore, fetcher, rng, theStore.getProductsByCategory(lcboProdCategory), category, lcboProdCategory, requestSize)

    }
  }
}
