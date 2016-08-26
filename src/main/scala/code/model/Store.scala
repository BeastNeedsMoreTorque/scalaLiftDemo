package code.model

import cats.data.Xor
import code.model.GlobalLCBO_IDs.{LCBO_KEY, P_KEY}
import code.model.prodAdvisor.{MonteCarloProductAdvisorComponentImpl, ProductAdvisorDispatcher, SlowAdvisorComponentImpl}
import code.model.utils.RetainSingles.asMap
import net.liftweb.json._
import net.liftweb.record.MetaRecord
import net.liftweb.record.field._
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props
import org.squeryl.Table
import org.squeryl.annotations._
import scala.collection._
import scala.collection.concurrent.TrieMap
import scala.language.implicitConversions
import scala.xml.Node
import code.model.GlobalLCBO_IDs._
import code.model.utils.RNG
import scala.util.Random

trait StoreSizeConstants {
  def nameSize: Int = Props.getInt("store.size.NAME", 0)
  def addressSize: Int = Props.getInt("store.size.ADDRESS", 0)
  def cityNameSize: Int = Props.getInt("store.size.CITY_NAME", 0)
}

trait EventTypes {
  /**
    * Selection is an iterable of product along with a count of many there are
    */
  type Selection = Iterable[(IProduct, Long)]

  /**
    * captures exceptions as errors in Xor if any, otherwise a selection
    */
  type ValidateSelection = Xor[Throwable, Selection]

  /**
    * captures exceptions as errors in Xor if any, otherwise the quantity that got purchased
    */
  type ValidatePurchase = Xor[Throwable, Long]
}

case class Store private() extends LCBOEntity[Store] with IStore with StoreSizeConstants with StoreCacheService with EventTypes {
  // products is a StatefulManyToMany[Product,Inventory], it extends Iterable[Product]
  lazy val storeProducts = MainSchema.inventories.leftStateful(this)
  @Column(name = "pkid")
  override val idField = new LongField(this, 0)
  // our own auto-generated id
  override val inventoryByProductIdMap: P_KEY => Option[Inventory] = key => inventoryByProductId.toMap.get(key)
  val lcbo_id = new LongField(this)
  // we don't share same PK as LCBO!
  val is_dead = new BooleanField(this, false)
  val latitude = new DoubleField(this)
  val longitude = new DoubleField(this)
  val name = new FilteredMandatoryStringField(nameSize)
  val address_line_1 = new FilteredMandatoryStringField(addressSize)
  val city = new FilteredMandatoryStringField(cityNameSize)
  override val productsCache = TrieMap[LCBO_KEY, IProduct]()
  override val categoryIndex = TrieMap[String, IndexedSeq[KeyKeeperVals]]()
  // don't put whole IProduct in here, just useful keys.
  override val inventoryByProductId = TrieMap[P_KEY, Inventory]()

  override def Name: String = name.get

  override def isDead: Boolean = is_dead.get

  override def addressLine1: String = address_line_1.get

  // for Loader and LCBOEntity
  override def table: Table[Store] = Store.table

  override def cache: concurrent.Map[P_KEY, Store] = Store.cache

  override def lcboKeyToPK: concurrent.Map[LCBO_KEY, P_KEY] = Store.lcboKeyToPKMap

  override def pKey: P_KEY = idField.get.PKeyID

  override def meta: MetaRecord[Store] = Store

  // following three caches leverage ORM's stateful cache of storeProducts and inventories above
  // (which are not presented as map but as slower sequence;
  // we organize as map for faster access).
  // They're recomputed when needed by the three helper functions that follow.
  override def getProduct(x: LCBO_KEY): Option[IProduct] = productsCache.get(x)

  override def getProductKeysByCategory(lcboCategory: String): IndexedSeq[KeyKeeperVals] =
    categoryIndex.get(lcboCategory).
      fold(IndexedSeq[KeyKeeperVals]()){ identity }

  def refreshProducts(): Unit =  {
    refreshInventories()
    addToCaches(storeProducts.toIndexedSeq)
  }

  override def refreshInventories(): Unit = inTransaction {
    storeProducts.refresh // key for whole inventory caching to work!
    inventoryByProductId ++= getInventories
  }

  private def getInventories: Map[P_KEY, Inventory] =
    asMap(inventories, { i: Inventory => i.productid.PKeyID} )

  override def inventories: Iterable[Inventory] = storeProducts.associations

  def advise(category: String, requestSize: Int, runner: ProductRunner): ValidateSelection = {
    Store.advise(this, category, requestSize, runner)
  }

  def consume(user: User, p: IProduct, quantity: Long): ValidatePurchase =
    Store.consume(this, user, p, quantity)

  override def asyncLoadCache(): Unit =
  // A kind of guard: Two piggy-backed requests to loadCache for same store will thus ignore second one.
    if (Store.storeProductsLoaded.putIfAbsent(idField.get, Unit).isEmpty) loadCache()

  override def lcboKey: LCBO_KEY = lcbo_id.get.LcboKeyID

  case class CategoryKeyKeeperVals(category: String, keys: KeyKeeperVals)

  /**
    * contains configuration values as to whether we want to use a random see and if instead we use a fixed one its value
    * the FixedRNGSeed is to enable unit testing.
    */
  object Shuffler {
    /**
      * When true we use standard (non functional have side effect) random number generation, but when false we use value of FixedRNGSeed.
      * When false, FixedRNGSeed's value is ignored.
      */
    val UseRandomSeed = Props.getBool("productInteraction.useRandomSeed", true)
    /**
      * the seed to use when we want to bypass standard rand and control the random number generation
      */
    val FixedRNGSeed = Props.getInt("productInteraction.fixedRNGSeed", 0)
  }

}

object Store extends Store with MetaRecord[Store] {
  val agentInterface = Props.get("store.agentInterface", "MonteCarloProductAdvisorComponentImpl")
  val advisor = agentInterface match {
    case "MonteCarloProductAdvisorComponentImpl" => new ProductAdvisorDispatcher with MonteCarloProductAdvisorComponentImpl
    case _ => new ProductAdvisorDispatcher with SlowAdvisorComponentImpl
  }

  def advise(invService: InventoryService,
             category: String,
             requestSize: Int,
             runner: ProductRunner): ValidateSelection = {
    val rng = RNG.Simple(if (Shuffler.UseRandomSeed) Random.nextInt() else Shuffler.FixedRNGSeed)
    advisor.advise(invService, category, requestSize, runner)(rng)
  }

  def consume(invService: InventoryService,
              user: User,
              p: IProduct,
              quantity: Long): ValidatePurchase =
    advisor.consume(invService, user, p, quantity)

  override val cache: concurrent.Map[P_KEY, Store] = TrieMap() // primary cache
  val lcboKeyToPKMap: concurrent.Map[LCBO_KEY, P_KEY] = TrieMap() // secondary dependent cache, a.k.a. index
  val queryFilterArgs = getSeq("store.query.Filter")(ConfigPairsRepo.defaultInstance) :+ "per_page" -> Props.getInt("store.lcboMaxPerPage", 0)
  private val storeProductsLoaded: concurrent.Map[Long, Unit] = TrieMap()

  def findAll: Iterable[Store] = cache.values

  def storeIdTolcboKey(pKey: P_KEY): Option[LCBO_KEY] = getStore(pKey).map(_.lcboKey)

  def getStore(pKey: P_KEY): Option[Store] = cache.get(pKey)
  // effectively a thread-safe lock-free set, which helps avoiding making repeated requests for cache warm up for a store.

  /* Convert a store to XML, @see Scala in Depth implicit view */
  implicit def toXml(s: Store): Node =
    <store>{Xml.toXml(s.asJValue)}</store>

  /**
    * synchronous because once the webapp accepts requests, this load must have completed so that the store collection is not empty.
    * Well... Trend is to do everything asynch these days...
    */
  override def load(): Unit = inTransaction {
    def context( ex: String): String =
      s"Problem loading LCBO stores into cache with exception error '$ex'"

    lazy val onEmptyError = "Problem loading LCBO stores into cache, none found"

    logger.info("load start")
    val dbStores = from(table)(item => select(item))
    cacheItems(dbStores)
    // the initial db select is long and synchronous, long because of loading Many-to-Many stateful state, depending on stored data
    val refreshed = getStores  // improves our cache of stores with latest info from LCBO. In real-world,
    // we might have the app run for long and call getStores async once in a while
    refreshed.toOption.fold[Unit](
      refreshed.leftMap(f => logger.error(context(f.toString()))))(
      items => {
        if (items.isEmpty) logger.error(onEmptyError)
        synchDirtyAndNewItems(items, getCachedItem)
      })
    logger.info("load end") // trace because it takes a long time.
  }

  override def table: Table[Store] = MainSchema.stores

  override def cacheItems(items: Iterable[Store]): Unit = {
    super.cacheItems(items)
    cache.values.foreach( _.refreshProducts())  // ensure inventories are refreshed INCLUDING on start up.
  }

  def getCachedItem: IStore => Option[IStore] = s =>
    for {pKey <- lcboKeyToPKMap.get(s.lcboKey)
         is <- getStore(pKey)} yield is

  private def getStores = collectItemsAsWebClient("/stores", extract, queryFilterArgs)
    // nice to know if it's empty, so we can log an error in that case.
}
