package code.model

import scala.collection._
import scala.language.implicitConversions
import scala.util.{Failure, Random, Success}
import scala.xml.Node
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import net.liftweb.common.Box
import net.liftweb.json._
import net.liftweb.util.Helpers.tryo
import net.liftweb.record.MetaRecord
import net.liftweb.record.field._
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props
import org.squeryl.annotations._
import code.model.Product.{fetchByStore, fetchByStoreCategory}
import code.model.Inventory.fetchInventoriesByStore
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.utils.Combinator

class Store private() extends LCBOEntity[Store] with IStore {

  @Column(name="pkid")
  override val idField = new LongField(this, 0)  // our own auto-generated id
  val lcbo_id = new LongField(this) // we don't share same PK as LCBO!

  // for Loader and LCBOEntity
  override def table(): org.squeryl.Table[Store] = Store.table()
  override def cache() = Store.storesCache
  override def LcboIdToPK() = Store.LcboIdToPK
  override def pKey: P_KEY = P_KEY(idField.get)
  override def lcboId: LCBO_ID = LCBO_ID(lcbo_id.get)
  override def meta = Store

  override def getCachedItem: (IStore) => Option[IStore] =  s => Store.getItemByLcboId(s.lcboId)

  val is_dead = new BooleanField(this, false)
  val latitude = new DoubleField(this)
  val longitude = new DoubleField(this)
  val name = new StringField(this, 200) {
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val address_line_1 = new StringField(this, 200) {
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val city = new StringField(this, 30) {
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }

  override def Name = name.get
  override def isDead = is_dead.get
  override def addressLine1 = address_line_1.get

  override def canEqual(other: Any) =
    other.isInstanceOf[Store]

  override def equals(other: Any): Boolean =
    other match {
      case that: Store =>
        if (this eq that) true
        else {
          that.canEqual(this) &&
          (Name == that.Name &&
           isDead == that.isDead &&
           addressLine1 == that.addressLine1 )
        }
      case _ => false
    }

  //products is a StatefulManyToMany[Product,Inventory], it extends Iterable[Product]
  lazy val storeProducts = MainSchema.inventories.leftStateful(this)
  private def inventories = storeProducts.associations

  // following three caches leverage ORM's stateful cache of storeProducts and inventories above (which are not presented as map but as slower sequence;
  // we organize as map for faster access).
  // They're recomputed when needed by the three helper functions that follow.
  private val productsCache = TrieMap[LCBO_ID, IProduct]()  // keyed by lcboId (comment is obsolete as now enforced by type system, woo-hoo!)
  private val productsCacheByCategory = TrieMap[String, IndexedSeq[IProduct]]()
  private val inventoryByProductId = TrieMap[P_KEY, Inventory]()
  private val getCachedInventoryItem: (Inventory) => Option[Inventory] = { (inv: Inventory) => inventoryByProductId.get(P_KEY(inv.productid)) }

  private def productsByLcboId: Map[LCBO_ID, Product] =
    storeProducts.toIndexedSeq.groupBy(_.lcboId).mapValues(_.head)

  private def productsByCategory: Map[String, IndexedSeq[Product]] =
    storeProducts.toIndexedSeq.groupBy(_.primaryCategory)

  private def getInventories: Map[P_KEY, Inventory] =
    inventories.toIndexedSeq.map { inv => P_KEY(inv.productid) -> inv } (breakOut)  // moderately slow because of iteration

  private def addToCaches(items: IndexedSeq[IProduct]) = {
    productsCache ++= items.groupBy(_.lcboId).mapValues(_.head) // update local store specific caches after having updated global cache for all products
    productsCacheByCategory ++= items.groupBy(_.primaryCategory)
  }

  private def emptyInventory =
    inventories.toIndexedSeq.forall(_.quantity == 0)

  /**
    * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
    * Select a random product that matches the parameters subject to a max sample size.
    *
    * @param category a String such as beer, wine, mostly matching primary_category at LCBO, or an asset category (for query only not to compare results and filter!).
    * @param requestSize a number representing how many items we need to sample
    * @return quantity found in inventory for product and the product
    */
  def recommend(category: String, requestSize: Int): Box[Iterable[(IProduct, Long)]] = {
    /**
      *
      * @param lcboProdCategory specifies the expected value of primary_category on feedback from LCBO. It's been known that they would send a Wiser's Whiskey on a wine request.
      * @return 0 quantity found in inventory for product (unknown to be resolved in JS) and the product
      */
    def getSerialResult(lcboProdCategory: String) = {
      val prods = fetchByStoreCategory(lcboId, category, Store.MaxSampleSize) // take a hit of one go to LCBO, querying by category, no more.
      // we accept to pay price of full shuffle here for two reasons:
      // 1) we had to pay price for web service so that's magnitudes slower than any in memory shuffling
      // 2) We already limited the sample size somewhat by choosing not to fetch all matching products immediately (Store.MaxSampleSize is not that big)
      val permutedIndices = Random.shuffle[Int, IndexedSeq](prods.indices)
      // stream avoids checking primary category on full collection (the permutation is done though).
      val stream = for (id <- permutedIndices.toStream;
                        p = prods(id) if p.primaryCategory == lcboProdCategory) yield p
      stream.take(requestSize).zip(Seq.fill(requestSize)(0.toLong))  // filter by category before take as LCBO does naive (or generic) pattern matching on all fields
      // and then zip with list of zeroes because we are too slow to obtain inventories.
    }

    // we could get errors going to LCBO, this tryo captures those.
    tryo {
      val lcboProdCategory = LiquorCategory.toPrimaryCategory(category) // transform to the category LCBO uses on product names in results (more or less upper case such as Beer)
      val matchingProds = productsCacheByCategory.get(lcboProdCategory).fold(IndexedSeq[IProduct]()){ identity }
      val inStockItems =
      { for ( p <- matchingProds;
              inv <- inventoryByProductId.get(p.pKey);
              q = inv.quantity if q > 0) yield (p, q)}

      // products are loaded before inventories and we might have none
      asyncLoadCache() // if we never loaded the cache, do it (fast lock free test). Note: useful even if we have product of matching inventory

      // here we try to sample a small subset randomly from a large set fast for the fun of it, nothing else.
      // Specifically if inStockItems is very large (thousands), we take a random sample of requestSize indices from that
      // and use those indices to select the random items. So to select up to 20 items out 2000 is fast, rather than shuffling 2000 indices.
      val cachedIds = Combinator.sampleNonNegativeShorts(inStockItems.indices.size, requestSize)
      if (cachedIds.nonEmpty) cachedIds map inStockItems  // get back the items that the ids have been selected (we don't stream because we know inventory > 0)
      else getSerialResult(lcboProdCategory)
    }
  }

  // generally has side effect to update database with more up to date content from LCBO's (if different)
  private def loadCache(): Unit = {
    def fetchProducts() = {
      val fullContextErr = { (m: String, err: String) =>
        s"Problem loading products into cache with message $m and exception error $err"
      }
      val theProducts = fetchByStore(lcboId)
      lazy val err = checkErrors(theProducts, fullContextErr, error = "Problem loading products into cache" )
      theProducts.toOption.fold[Unit](logger.error(err))( items => addToCaches _)
    }

    def fetchInventories() = {
      def inventoryTableUpdater: (Iterable[Inventory]) => Unit = MainSchema.inventories.update _
      def inventoryTableInserter: (Iterable[Inventory]) => Unit = MainSchema.inventories.insert _
      val fullContextErr = (m: String, err: String) =>
        s"Problem loading inventories into cache for '$lcboId' with message $m and exception error $err"
      // fetch @ LCBO, store to DB and cache into ORM stateful caches, trap/log errors, and if all good, refresh our own store's cache.
      // we chain errors using flatMap (FP fans apparently like this).
      val box =
        fetchInventoriesByStore(
          webApiRoute = "/inventories",
          getCachedInventoryItem,
          inventoryByProductId.toMap,
          Seq("store_id" -> lcboId, "where_not" -> "is_dead")).
      flatMap { inventories =>
        inTransaction {
          execute[Inventory](inventories.updatedInvs, inventoryTableUpdater) // bulk update the ones needing an update, having made the change from LCBO input
          execute[Inventory](inventories.newInvs, inventoryTableInserter) // bulk insert the ones needing an insert having filtered out duped composite keys
        }
      }
      lazy val err = checkUnitErrors(box, fullContextErr )
      box.toOption.fold[Unit](logger.error(err))( (Unit) => refreshInventories())
    }

    val fetches =
      for (p <- Future(fetchProducts); // fetch and then make sure model/Squeryl classes update to DB and their cache synchronously, so we can use their caches.
           i <- Future(fetchInventories)) yield i // similarly for inventories and serialize intentionally because of Ref.Integrity  if no exception was thrown

    fetches onComplete {
      case Success(_) => //We've persisted along the way for each LCBO page ( no need to refresh because we do it each time we go to DB)
        logger.debug(s"loadCache async work succeeded for $lcboId")
        if (emptyInventory) logger.warn(s"got no product inventory for storeId $lcboId !") // No provision for retrying.
      case Failure(f) => logger.info(s"loadCache explicitly failed for $lcboId cause ${f.getMessage}")
    }
    logger.info(s"loadCache async launched for $lcboId") // about 15 seconds, likely depends mostly on network/teleco infrastructure
  }

  private def asyncLoadCache() =
    // A kind of guard: Two piggy-backed requests to loadCache for same store will thus ignore second one.
    if (Store.storeProductsLoaded.putIfAbsent(idField.get, Unit).isEmpty) loadCache()

  def refreshInventories(): Unit = inTransaction {
    storeProducts.refresh // key for whole inventory caching to work!
    inventoryByProductId ++= getInventories
  }

  def refreshProducts(): Unit = inTransaction {
    storeProducts.refresh // key for whole inventory caching and products caching to work!
    productsCache ++= productsByLcboId
    productsCacheByCategory ++= productsByCategory
    inventoryByProductId ++= getInventories
  }
}

object Store extends Store with MetaRecord[Store] {
  def findAll(): Iterable[Store] = storesCache.values

  private val MaxSampleSize = Props.getInt("store.maxSampleSize", 0)

  private val storesCache: concurrent.Map[P_KEY, Store] = TrieMap()  // primary cache
  override val LcboIdToPK: concurrent.Map[LCBO_ID, P_KEY] = TrieMap() //secondary dependent cache
  override def table() = MainSchema.stores

  override def cacheItems(items: Iterable[Store]): Unit = {
    super.cacheItems(items)
    storesCache.values.foreach( _.refreshProducts())  // ensure inventories are refreshed INCLUDING on start up.
  }

  private val storeProductsLoaded: concurrent.Map[Long, Unit] = TrieMap()
  // effectively a thread-safe lock-free set, which helps avoiding making repeated requests for cache warm up for a store.

  val queryFilterArgs = getSeq("store.query.Filter")(ConfigPairsRepo.defaultInstance) :+ "per_page" -> Props.getInt("store.lcboMaxPerPage", 0)

  override def getCachedItem: (IStore) => Option[IStore] = s => getItemByLcboId(s.lcboId)
  def availableStores = storesCache.keySet
  def lcboIdToPK(id: LCBO_ID): Option[P_KEY] = LcboIdToPK.get(id)
  def storeIdToLcboId(s: P_KEY): Option[LCBO_ID] = storesCache.get(s).map(_.lcboId)
  def getStore(id: P_KEY) = storesCache.get(id)
  def getItemByLcboId(id: LCBO_ID) =
    for (dbId <- LcboIdToPK.get(id);
         s <- storesCache.get(dbId)) yield s


    /* Convert a store to XML, @see Scala in Depth implicit view */
  implicit def toXml(st: Store): Node =
    <store>{Xml.toXml(st.asJValue)}</store>

  private def getStores(): Box[IndexedSeq[Store]] = tryo {
      collectItemsAsWebClient("/stores", extract, queryFilterArgs) // nice to know if it's empty, so we can log an error in that case. That's captured by box and looked at within checkErrors using briefContextErr.
    }

  /**
    * synchronous because once the webapp accepts requests, this load must have completed so that the store collection is not empty.
    * Well... Trend is to do everything asynch these days...
    */
  override def load(): Unit = inTransaction {
    def fullContextErr( m: String,  ex: String): String =
      s"Problem loading LCBO stores into cache with message '$m' and exception error '$ex'"
    def briefContextErr(): String =
      "Problem loading LCBO stores into cache, none found"

    logger.info("load start")
    val dbStores = from(table())(item => select(item))
    cacheItems(dbStores)
    // the initial db select is long and synchronous, long because of loading Many-to-Many stateful state, depending on stored data
    val refreshed = getStores()  // improves our cache of stores with latest info from LCBO. In real-world, we might have the app run for long and call getStores async once in a while
    lazy val err = checkErrors(refreshed, fullContextErr, briefContextErr )
    refreshed.toOption.fold[Unit](logger.error(err))( items => synchDirtyAndNewItems(items, getCachedItem, dirtyPredicate))
    logger.info("load end")
  }
}