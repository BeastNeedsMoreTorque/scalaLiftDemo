package code.model

import java.io.IOException
import java.sql.SQLException

import org.squeryl.dsl.StatefulOneToMany

import scala.annotation.tailrec
import scala.collection._
import scala.language.implicitConversions
import scala.util.Random
import scala.xml.Node
import scala.collection.concurrent.TrieMap
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import net.liftweb.common.{Full, Empty, Box, Loggable}
import net.liftweb.json._
import net.liftweb.json.JsonParser.{ParseException, parse}
import net.liftweb.util.Helpers.tryo
import net.liftweb.record.{MetaRecord, Record}
import net.liftweb.record.field._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props

import org.squeryl.annotations._

import code.model.Product._
import code.Rest.pagerRestClient
import MainSchema._

/**
  * Created by philippederome on 15-11-01.
  * This is captured from JSON parsing.
  */
case class StoreAsLCBOJson (id: Int = 0,
                 is_dead: Boolean = true,
                 latitude: Double = 0.0,
                 longitude: Double = 0.0,
                 name: String = "",
                 address_line_1: String = "",
                 city: String = "",
                 distance_in_meters: Int = 0) {
  def this(s: Store) = this(s.lcboId.toInt, false, s.latitude.get, s.longitude.get, s.name.get, s.address_line_1.get, s.city.get  )

  // intentional aliasing allowing more standard naming convention.
  def isDead = is_dead

  override def toString = s"$id, name: $name, Address: $address_line_1, city: $city, distance is:$distanceInKMs"

  // intentional change of scale from metres to kilometres, using String representation instead of integer and keeping 3 decimals (0.335 ml for beer)
  def distanceInKMs: String = {
    val d = distance_in_meters / 1000.0
    f"$d%1.1f KM(s)"
  }

  /**
    *
    * @return an ordered list of pairs of values (label and value), representing most of the interesting data of the product
    */
  def createProductElemVals: IndexedSeq[Attribute] =
    (Attribute("Name: ", name) ::
     Attribute ("Primary Address: ", address_line_1) ::
     Attribute("City: ", city) ::
     Attribute("Your Distance: ", distanceInKMs) ::
     Attribute("Latitude: ", latitude.toString) ::
     Attribute("Longitude: ", longitude.toString) ::
     Nil).filterNot{ attr =>  attr.value == "null" || attr.value.isEmpty }.toVector

}

object StoreAsLCBOJson {
  private implicit val formats = net.liftweb.json.DefaultFormats

  def apply(s: Store) = new StoreAsLCBOJson(s)

  /**
    * Convert a store to XML
    */
  implicit def toXml(st: StoreAsLCBOJson): Node =
    <store>{Xml.toXml(st)}</store>

  /**
    * Convert the store to JSON format.  This is
    * implicit and in the companion object, so
    * a Store can be returned easily from a JSON call
    */
  implicit def toJson(st: StoreAsLCBOJson): JValue =
    Extraction.decompose(st)

}

case class PlainStoreAsLCBOJson (id: Int = 0,
                                 is_dead: Boolean = true,
                                 latitude: Double = 0.0,
                                 longitude: Double = 0.0,
                                 name: String = "",
                                 address_line_1: String = "",
                                 city: String = "") {

  def getStore(dbStores: Map[Long, Store]): Option[Store] = {
    for (storeId <- Store.lcboIdToDBId(id);
         s <- dbStores.get(storeId)
    ) yield s
  }

}

class Store private() extends Record[Store] with KeyedRecord[Long] with CreatedUpdated[Store]  {
  //products is a ManyToMany[Product,Inventory], it extends Query[Product]
  lazy val storeProducts = MainSchema.inventories.left(this)
  def productsByCategory: Map[String, IndexedSeq[Product]] = storeProducts.toIndexedSeq.groupBy(_.primaryCategory)
  lazy val inventories: StatefulOneToMany[Inventory] = MainSchema.storeToInventories.leftStateful(this)
  def inventoriesByCategory: Map[String, IndexedSeq[Inventory]] = inventories.toIndexedSeq.
    groupBy { inv: Inventory =>
      val p = storeProducts.where(p => p.id === inv.productid).headOption
      p.fold("") { p: Product => p.primaryCategory }
    }

  def inventoriesPerProductInStore: Map[Long, Inventory] = inventories.toIndexedSeq.
    map { inv: Inventory => inv.productid -> inv } (breakOut)


  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id
  val lcbo_id = new LongField(this) // we don't share same PK as LCBO!
  def lcboId: Long = lcbo_id.get
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

  def meta = Store

  def copyAttributes(my: Store, s: PlainStoreAsLCBOJson): Store = {
    my.is_dead.set(s.is_dead)
    my.address_line_1.set(s.address_line_1)
    my
  }

  def isDirty(s: PlainStoreAsLCBOJson): Boolean = {
    is_dead.get != s.is_dead ||
    address_line_1.get != s.address_line_1
  }
}

object Store extends Store with MetaRecord[Store] with pagerRestClient with Loggable {
  private implicit val formats = net.liftweb.json.DefaultFormats
  private val MaxSampleSize = Props.getInt("store.maxSampleSize", 0)
  private val DBBatchSize = Props.getInt("store.DBBatchSize", 1)

  private val storeLoadBatchSize = Props.getInt("store.loadBatchSize", 10)

  private val productCacheSize = Props.getInt("product.cacheSize", 10000)
  private val synchEmptiesFirst = Props.getBool("store.synchEmptiesFirst", false)

  private val storeProductsLoaded: concurrent.Map[Long, Unit] = TrieMap()
  // effectively a thread-safe lock-free set, which helps avoiding making repeated requests for cache warm up for a store.

  private val storesCache: concurrent.Map[Long, Store] = TrieMap[Long, Store]()
  private val LcboIdsToDBIds: concurrent.Map[Long, Long] = TrieMap[Long, Long]()
  def availableStores: Set[Long] = storesCache.toMap.keySet
  def lcboIdToDBId(l: Int): Option[Long] = LcboIdsToDBIds.get(l)
  def storeIdToLcboId(s: Long): Option[Long] = storesCache.get(s).map(_.lcboId)

  override def MaxPerPage = Props.getInt("store.lcboMaxPerPage", 0)
  override def MinPerPage = Props.getInt("store.lcboMinPerPage", 0)

  @volatile
  var dummy: Any = _
  def timed[T](body: =>T): Double = {
    val start = System.nanoTime
    dummy = body
    val end = System.nanoTime
    ((end - start) / 1000) / 1000.0
  }

  def hasProductsByStoreCategory(dbStoreId: Option[Long], category: String): Boolean = {
    val res =
      for (s <- dbStoreId.toSeq;
           store <- storesCache.get(s).toSeq;
           inv <- transaction { store.inventories};
           prod <- Product.getProduct(inv.productid) if prod.primaryCategory == category
      ) yield inv
    res.nonEmpty
  }

  def hasCachedProducts(dbStoreId: Long): Boolean =
    storesCache.get(dbStoreId).exists { id => id.inventories.nonEmpty }

  def getProductIdsByStoreCategory(dbStoreId: Long, category: String): IndexedSeq[Long] = {
    storesCache.get(dbStoreId).fold(IndexedSeq[Long]()) { s: Store =>
      for (inv <- transaction {s.inventories.toIndexedSeq};
           prod <- Product.getProduct(inv.productid) if prod.primaryCategory == category
      ) yield inv.productid
    }
  }

  def emptyInventoryForStore(dbStoreId: Long): Boolean =
    // if that store does not exist in cache, it's false.
    storesCache.get(dbStoreId).exists { s =>
      transaction { s.inventories }.forall(_.quantity == 0 )}


  def getInventories(dbStoreId: Long): Map[Long, Inventory] = {
    storesCache.get(dbStoreId).
      fold( Map[Long, Inventory]() )
      { _.inventories.toSeq.map (inv => inv.productid -> inv ) (breakOut) }
  }

  def getInventory(dbStoreId: Long, prodId: Long): Option[Inventory] = {
    for (s <- storesCache.get(dbStoreId);
         inv <- s.inventoriesPerProductInStore.get(prodId))
      yield inv
  }

  def getInventoryQuantity(dbStoreId: Long, prodId: Long): Option[Long] = {
    for (s <- storesCache.get(dbStoreId);
         inv <- s.inventoriesPerProductInStore.get(prodId))
      yield inv.quantity
  }

  def fetchItems(state: EntityRecordState,
                 storesByState: Map[EntityRecordState, IndexedSeq[PlainStoreAsLCBOJson]],
                 f: PlainStoreAsLCBOJson => Option[Store] ): IndexedSeq[Store] =
    storesByState.getOrElse(state, Vector()).flatMap{ f(_)}

  def asyncLoadCache(storeId: Long): Unit = {
    // A kind of guard: Two piggy-backed requests to loadCache for same store will thus ignore second one.
    // Slightly unwanted consequence is that clients need to test for empty set and not assume it's non empty.
    // we impose referential integrity so we MUST get products and build on that to get inventories that refer to products
    // Note: we may execute this function, get nothing back from LCBO (e.g. website down) and still provide user data because of our db store.
    if ( storeProductsLoaded.putIfAbsent(storeId, Unit).isEmpty) {
      val fut = Future { loadCache(storeId) }
      fut foreach {
        case m =>
          logger.debug(s"loadCache succeeded for $storeId")
          if (emptyInventoryForStore(storeId)) {
            logger.warn(s"got NO product inventory for storeId $storeId !") // No provision for retrying.
          }
      }
      fut.failed foreach {
        case f =>
          logger.info(s"loadCache explicitly failed for $storeId cause $f")
      }
    }
  }

  def init() = {

    def getStores(dbStores: Map[Long, Store]): Map[Long, Store] = {
      def synchronizeData(dbStores: Map[Long, Store]): Box[Map[Long, Store]] = {
        // collects stores individually from LCBO REST as PlainStoreAsLCBOJson on as many pages as required.
        // we declare types fairly often in the following because it's not trivial to follow otherwise
        @tailrec
        def collectStoresOnAPage(dbStores: Map[Long, Store],
                                 accumItems: Map[EntityRecordState, IndexedSeq[Store]],
                                 urlRoot: String,
                                 pageNo: Int): Map[EntityRecordState, IndexedSeq[Store]] = {
          val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo)
          val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
          val jsonRoot = parse(pageContent) // fyi: throws ParseException
          val itemNodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
          // get all the stores from JSON itemNodes, extract them and map them to usable Store class after synching it with our view of same record in database.
          val pageStores = {for (p <- itemNodes) yield p.extractOpt[PlainStoreAsLCBOJson]}.flatten

          // partition pageStoreSeq into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
          val storesByState: Map[EntityRecordState, IndexedSeq[PlainStoreAsLCBOJson]] = pageStores.groupBy {
            s =>  ( LcboIdsToDBIds.get(s.id).flatMap( dbStores.get ), s)  match {
              case (None, _) => New
              case (Some(store), lcboStore) if store.isDirty(lcboStore) => Dirty
              case (_ , _) => Clean  // or decided not to handle such as stores "out of bound" that we won't cache.
            }
          }

          val cleanStores = accumItems(Clean) ++ fetchItems(Clean, storesByState, s => s.getStore(dbStores))
          val dirtyStores = accumItems(Dirty) ++ fetchItems(Dirty, storesByState, s => s.getStore(dbStores).map(copyAttributes(_, s) ))
          val newStores = accumItems(New) ++ storesByState.getOrElse(New, Nil).map{ create }

          // after preliminaries, get the map of stores indexed properly by state that we need having accumulated over the pages so far.
          val revisedAccumItems: Map[EntityRecordState, IndexedSeq[Store]] = Map(New -> newStores, Dirty -> dirtyStores, Clean -> cleanStores)

          val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)

          if (pageStores.isEmpty || isFinalPage) {
            logger.info(uri) // log only last one to be less verbose
            return revisedAccumItems // no need to look at more pages
          }
          collectStoresOnAPage(
            dbStores,
            revisedAccumItems,
            urlRoot,
            pageNo + 1) // union of this page with next page when we are asked for a full sample
        }

        // we'd like the is_dead ones as well to update state (but apparently you have to query for it explicitly!?!?)
        val url = s"$LcboDomainURL/stores?"
        tryo {
          // gather stores on this page (url) with classification as to whether they are new, dirty or clean
          val initMap = Map[EntityRecordState, IndexedSeq[Store]](New -> Vector(), Dirty -> Vector(), Clean -> Vector())
          val lcboStores = collectStoresOnAPage(dbStores, initMap, url, 1)
          logger.debug(s"done loading to LCBO")

          // identify the dirty and new stores for batch update and then retain all of them as the requested set
          inTransaction {
            // for the activity on separate thread for synchronizeData
            // batch update the database now.
            try { // getNextException is actually very useful, which is why we do try catch here
              updateStores(lcboStores(Dirty))
              insertStores(lcboStores(New))
            } catch {
              case se: SQLException =>
                logger.error(s"SQLException")
                logger.error("Code: " + se.getErrorCode)
                logger.error("SqlState: " + se.getSQLState)
                logger.error("Error Message: " + se.getMessage)
                logger.error("NextException:" + se.getNextException)
              case e: Exception =>
                logger.error("General exception caught: " + e)
            }
          }
          lcboStores.values.flatten.map(s => s.id -> s)(breakOut) // flatten the 3 lists and then build a map from the stores keyed by id.
        }
      }

      import net.liftweb.common.Failure
      synchronizeData( dbStores) match {
          case Full(m) => m
          case Failure(m, ex, _) =>
            logger.error(s"Problem loading LCBO stores into cache with message '$m' and exception error '$ex'")
            dbStores
          case Empty =>
            logger.error(s"Problem loading LCBO stores into cache, none found")
            dbStores
      }
    }

    logger.info(s"Store.init start")
    Product.init()
    // load all stores from DB for navigation and synch with LCBO for possible delta (small set so we can afford synching, plus it's done async way)
    val dbStores: Map[Long, Store] = inTransaction {
      from(stores)(s =>
          select (s)).map(s => s.id -> s)(breakOut)
    }  // queries store table and throw it into map
    LcboIdsToDBIds ++= dbStores.map{ case (k,v) => v.lcboId -> k }
    storesCache ++= getStores(dbStores)

    logger.info("Store.init end")
  }

  /**
    * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
    * Select a random product that matches the parameters subject to a max sample size.
    *
    * @param store a String representing a numeric code of a LCBO store
    * @param category a String such as beer, wine, mostly matching primary_category at LCBO, or an asset category.
    * @return
    */
  def recommend(lcbo_storeId: Long, category: String, requestSize: Int): Box[Iterable[(Long, Product)]] = {
    def randomSampler(storeId: Long, prodKeys: IndexedSeq[Long]) = {
      val prodIds = Random.shuffle(prodKeys)
      // generate double the keys and hope it's enough to have nearly no 0 inventory as a result
      val seq =
        for ( id <- prodIds;
              p <- Product.getProduct(id);
              qty <- getInventoryQuantity(storeId, id) if qty > 0 ) yield (qty, p)
      seq.take(requestSize)
    }

    // note: cacheSuccess (top level code) grabs a lock
    def cacheSuccess(storeId: Option[Long], lcbo_storeId: Long, category: String): Option[Iterable[(Long, Product)]] = {
      // note: we never delete from cache, so we don't worry about data disappearing in a potential race condition

      if ( storeId.exists{ s => storeProductsLoaded.contains(s)} && hasProductsByStoreCategory(storeId, category) ) {
        storeId.map(s => randomSampler(s, getProductIdsByStoreCategory(s, category)))
      }
      else None
    }

    tryo { // we could get errors going to LCBO, this tryo captures those.
      val storeId: Option[Long] = LcboIdsToDBIds.get(lcbo_storeId)
      cacheSuccess(storeId, lcbo_storeId, LiquorCategory.toPrimaryCategory(category)).map { identity} getOrElse {
        logger.warn(s"recommend cache miss for $storeId") // don't try to load it asynchronously as that's start up job to get going with it.
        storeId.map(id => asyncLoadCache(id) )
        val prods: IndexedSeq[Product] = storeId.map(s => productsByStoreCategory(s, category)).
            fold(IndexedSeq[Product]()){identity} // take a hit of one go to LCBO, no more.
        val indices = Random.shuffle[Int, IndexedSeq](prods.indices )
        // generate double the keys and hope it's enough to have nearly no 0 inventory as a result
        storeId.fold(Seq[(Long, Product)]()) { s: Long =>
          val seq: IndexedSeq[(Long, Product)] = {
            // just use 0 as placeholder for inventory for now as we don't have this info yet.
            for (idx <- indices;
                 p = prods(idx)) yield (0.toLong, p)
          }
          // we may have 0 inventory, browser should try to finish off that work not web server.
          seq.take(requestSize)
        }
      }
    }
  }

  def create(s: PlainStoreAsLCBOJson): Store = {
    // store in same format as received by provider so that un-serializing if required will be same logic. This boiler-plate code seems crazy (not DRY at all)...
    createRecord.
      lcbo_id(s.id).
      name(s.name).
      is_dead(s.is_dead).
      address_line_1(s.address_line_1).
      city(s.city).
      latitude(s.latitude).
      longitude(s.longitude)
  }

  def findAll(): Iterable[StoreAsLCBOJson] =
    storesCache.values.map( StoreAsLCBOJson(_))

  def updateStores(myStores: Iterable[Store]) =
    myStores.grouped(DBBatchSize).
      foreach{ stores.update }

  def insertStores( myStores: Iterable[Store]) =
    myStores.grouped(DBBatchSize).
      foreach{ stores.insert }

  // may have side effect to update database with more up to date from LCBO's content (if different)
  def loadCache(storeId: Long): Unit = {

    def getProductsOfStartPage: Seq[Product] = {
      def loadAll(storeId: Long): Box[Map[EntityRecordState, IndexedSeq[Product]]] =
        tryo { getProductsByStore(storeId) }

      inTransaction {
        loadAll(storeId) match {
          case Full(m) => // Used to be 10 seconds to get here after last URL query, down to 0.046 sec
            // 5 seconds per thread (pre Jan 23 with individual db writes). Now 1.5 secs for a total of 2-3 secs, compared to at least 15 secs.
            m.values.flatten.toSeq // flatten the 3 lists
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading products into cache for '$storeId' with message $m and exception error $ex")
            List[Product]()
          case Empty =>
            logger.error(s"Problem loading products into cache for '$storeId'")
            List[Product]()
        }
      }
    }

    def getInventoriesOfStartPage: Iterable[Inventory] = {
      def loadAllInventories(): Box[Iterable[Inventory]] =
        tryo {inventoriesByStore(storeId) }

      inTransaction {
        loadAllInventories() match {
          case Full(m) => m
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading inventories into cache for '$storeId' with message $m and exception error $ex")
            Seq[Inventory]()
          case Empty =>
            logger.error(s"Problem loading inventories into cache for '$storeId'")
            Seq[Inventory]()
        }
      }
    }

    logger.debug(s"loadCache start $storeId")
    // fetch and then make sure model/Squeryl classes update to DB and their cache synchronously, so we can use their caches.
    storesCache.get(storeId).foreach { s =>
      Product.update(getProductsOfStartPage) // refresh product cache, still explicit, not Squeryl way.
      val invs = getInventoriesOfStartPage
      inTransaction {
        invs.foreach(inv => s.inventories.associate(inv))
        s.inventories.refresh
      }
    }
    logger.debug(s"loadCache ended $storeId") // 30 seconds from last LCBO query to completing the cache update (Jan 23). Needs to be better.
  }

  import java.net.SocketTimeoutException
  // for reflection and generating documentation
  private final def getSingleStore( uri: String): PlainStoreAsLCBOJson = {
    logger.debug(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    (parse(pageContent) \ "result").extract[PlainStoreAsLCBOJson] // more throws
  }

  /**
    * Queries LCBO matching category and storeId for a sample size as specified by client, with category considered optional, though not tested when optional.
    * Full URL will be built as follows: http://lcbo.com/products?store_id=<storeId>&q=<category.toLowerCase()>&per_page=<perPage>
    * LCBO allows to specify q as query to specify pattern match on product name (e.g. beer, wine)
    * for pattern match LCBO uses lower case but for actual product category it's upper case, so to make comparisons, we will need to account for that
    * primary_category in catalog or p.primary_category so we need a conversion function to adjust)
    *
    * @param requiredSize upper bound on #items we need. Attempt to match it if enough supply is available.
    * @param store id  of Store at LCBO
    * @param category wine, spirits, and so on
    * @return collection of LCBO products while throwing.
    * @throws SocketTimeoutException timeout reached
    * @throws IOException I/O issue
    * @throws ParseException parse issue
    * @throws MappingException, etc
    */
  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  private def productsByStoreCategory(storeId: Long, category: String): IndexedSeq[Product] = {
    if (MaxSampleSize <= 0) Vector()
    else {
      val lcboStoreId = storeIdToLcboId(storeId)
      lcboStoreId.fold(IndexedSeq[Product]()) { s =>
        val url = s"$LcboDomainURL/products?store_id=$s" + additionalParam("q", category) // does not handle first one such as storeId, which is artificially mandatory
        val filter = { p: ProductAsLCBOJson => p.primary_category == LiquorCategory.toPrimaryCategory(category) &&
          !p.is_discontinued
        } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers
        val items = collectItemsOnAPage(
          Map[EntityRecordState, IndexedSeq[Product]](New -> Vector(), Dirty -> Vector(), Clean -> Vector()),
          url,
          MaxSampleSize,
          pageNo = 1,
          reconWithDB = false)
        items.values.take(MaxSampleSize).flatten.toVector // we don't care about Clean/New/Dirty state here so flatten values.
      }
    }
  }

  // Fetch both the original product as case class JSON-like from LCBO
  // as well as the DB Record we'd associate it with as Product and key that pair
  // with LCBO product id. The list is generated by a query scoped by the store id.
  // We also make a somewhat arbitrary assumption that discontinued products are of zero interest
  // So we use that filter on REST API call to LCBO.
  // The original product is retrieved because it has store-sensitive context such as inventory
  // which we want to inform user of but that does not belong to the "pure" Product itself.
  // The association from JSON-like LCBO product type to our db type is done with fetchSynched.
  // This picks up the product from database and brings it up to date.
  // Future work:
  //     do update in series not one-by-one for scalability.
  //     Make use of a product cache we already have, so don't go to database unconditionally naively.
  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  private def getProductsByStore(store: Long): Map[EntityRecordState, IndexedSeq[Product]] = {
    if (productCacheSize <= 0) Map()
    else {
      val initMap = Map[EntityRecordState, IndexedSeq[Product]](New -> Vector(), Dirty -> Vector(), Clean -> Vector())
      val lcbo_storeId = storeIdToLcboId(store)
      lcbo_storeId.fold(initMap) { s =>
        val url = s"$LcboDomainURL/products?store_id=$s"
        val filter = { p: ProductAsLCBOJson => !p.is_discontinued } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers

        collectItemsOnAPage(
          initMap,
          url,
          productCacheSize,
          1, // programmer client is assumed to know we use this as a page.
          filter).take(productCacheSize)
      }
    }
  }

  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  private def inventoriesByStore(storeId: Long): Iterable[Inventory] = {
    @throws(classOf[net.liftweb.json.MappingException])
    @throws(classOf[net.liftweb.json.JsonParser.ParseException])
    @throws(classOf[java.io.IOException])
    @throws(classOf[java.net.SocketTimeoutException])
    @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
    @tailrec
    def collectInventoriesOnAPage(accumItems: Seq[Inventory],
                                    urlRoot: String,
                                    requiredSize: Int,
                                    pageNo: Int,
                                    myFilter: InventoryAsLCBOJson => Boolean = { sp: InventoryAsLCBOJson => true }): Seq[Inventory] = {
      def fetchItems(state: EntityRecordState,
                     mapVector: Map[EntityRecordState, IndexedSeq[InventoryAsLCBOJson]],
                     f: InventoryAsLCBOJson => Option[Inventory] ): IndexedSeq[Inventory] = {
        mapVector.getOrElse(state, Vector()).flatMap { p => f(p) } // remove the Option in Option[Product]
      }

      def stateOfProduct(item: InventoryAsLCBOJson): EntityRecordState = {
        val invOption =
          for (dbProductId <- lcboidToDBId(item.product_id);
             i <- getInventory(storeId, dbProductId)) yield i
        invOption  match {
          case None  => New
          case Some(inv) if inv.isDirty(item) => Dirty
          case _ => Clean
        }
      }

      if (requiredSize <= 0) return accumItems
      // specify the URI for the LCBO api url for liquor selection
      val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo) // get as many as possible on a page because we could have few matches.
      val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
      val jsonRoot = parse(pageContent) // fyi: throws ParseException
      val itemNodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
      val items = (for (p <- itemNodes) yield p.extract[InventoryAsLCBOJson].removeNulls).filter(myFilter)  // LCBO sends us poisoned useless nulls that we need to filter for DB (filter them right away).
      val outstandingSize = requiredSize - items.size

      // Collects into our list of products the attributes we care about (extract[Product]). Then filter out unwanted data.
      // fyi: throws Mapping exception.
      //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
      val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
      val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)

      // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
      val storeProductsByState: Map[EntityRecordState, IndexedSeq[InventoryAsLCBOJson]] = items.toIndexedSeq.groupBy( stateOfProduct )

      val storeInventories = getInventories(storeId)
      def inventoryForStoreAndLCBOProd(lcboProdID: Int): Option[Inventory] = {
        val dbProdId = Product.lcboidToDBId(lcboProdID )
        dbProdId.map{ id: Long => storeInventories.get(id) }.getOrElse(None)
      }

      val dirtyInventories = fetchItems(Dirty, storeProductsByState, {inv => inventoryForStoreAndLCBOProd(inv.product_id).map { _.copyAttributes( inv)} } )
      // get the New partition, returning Nil if we don't have any, and open up option for a match on the productID in database from the LCBO ID.
      // finally fetch a Inventory that is created with specified productId, quantity, and storeId.
      val newInventories = storeProductsByState.getOrElse(New, Nil).
                flatMap { inv => lcboidToDBId(inv.product_id).
              map { dbProductId: Long => new Inventory(storeId, dbProductId, inv.quantity ) } } (collection.breakOut)
      val revisedAccumItems = accumItems ++ newInventories ++ dirtyInventories

      if (outstandingSize <= 0 || isFinalPage || totalPages < pageNo +1) {
        logger.info(uri) // log only last one to be less verbose
        return revisedAccumItems
      }
      // Deem as last page only if  LCBO tells us it's final page or we evaluate next page won't have any (when we gap due to parallelism).
      // Similarly having reached our required size,we can stop.

      // tail recursion enforced.
      collectInventoriesOnAPage(
        revisedAccumItems,
        urlRoot,
        outstandingSize,
        pageNo + 1,
        myFilter) // union of this page with next page when we are asked for a full sample
    }

    if (productCacheSize <= 0) Seq()
    else {
      val lcboStoreId = storeIdToLcboId(storeId)
      val initSeq = Seq[Inventory]()
      lcboStoreId.fold(initSeq) { s =>
        val url = s"$LcboDomainURL/inventories?store_id=$s"
        val filter = { p: InventoryAsLCBOJson => !p.is_dead } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers

        collectInventoriesOnAPage(
          initSeq,
          url,
          productCacheSize,
          1, // programmer client is assumed to know we use this as a page.
          filter).take(productCacheSize)
      }
    }
  }

  /**
    * LCBO client JSON query handler. So naturally, the code is specifically written with the structure of LCBO documents in mind, with tokens as is.
    * For Liftweb JSON extraction after parse,
    *
    * @see https://github.com/lift/lift/tree/master/framework/lift-base/lift-json/
    *      don't go to more pages than user implicitly requests via requiredSize that should not be exceeded.
    *      Would Streams collection be handy for paging here? Depends on consumption usage perhaps.
    *      Uses tail recursion.
    * @param accumItems accumulator to facilitate tail recursion
    * @param urlRoot a LCBO product query without the details of paging, which we handle here
    * @param requiredSize required size of products that are asked for. May get less if there are fewer matches, but will not go above that size.
    * @param pageNo client calls this with value 1 (initial page), recursion increments it, designates the pageno for LCBO JSON data when data fits on several pages
    * @param myFilter client's filter that can be applied as we process the data before mapping/extracting it out to client data.
    *                 In principle, should be faster when user filters reject many values, but no empirical evidence here.
    * @return a vector of product items matching the query and size constraint, though we may go a bit over the size by multiple of page sizes.
    * @throws java.net.SocketTimeoutException timeout is reached, slow connection
    * @throws java.io.IOException I/O issue
    * @throws net.liftweb.json.JsonParser.ParseException parse problem
    * @throws net.liftweb.json.MappingException our case class does not match JSon object from API
    *
    */
  @throws(classOf[net.liftweb.json.MappingException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @tailrec
  private final def collectItemsOnAPage(accumItems: Map[EntityRecordState, IndexedSeq[Product]],
                                        urlRoot: String,
                                        requiredSize: Int,
                                        pageNo: Int,
                                        myFilter: ProductAsLCBOJson => Boolean = { p: ProductAsLCBOJson => true },
                                        reconWithDB: Boolean = true): Map[EntityRecordState, IndexedSeq[Product]] = {
    def nextPage = pageNo + 1
    def fetchItems(state: EntityRecordState,
                   productsByState: Map[EntityRecordState, IndexedSeq[ProductAsLCBOJson]],
                   f: ProductAsLCBOJson => Option[Product] ): IndexedSeq[Product] = {
      productsByState.getOrElse(state, Vector()).flatMap { p => f(p) } // remove the Option in Option[Product]
    }

    if (requiredSize <= 0) return accumItems
    // specify the URI for the LCBO api url for liquor selection
    val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo) // get as many as possible on a page because we could have few matches.
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = (for (p <- itemNodes) yield p.extract[ProductAsLCBOJson].removeNulls).filter(myFilter)  // LCBO sends us poisoned useless nulls that we need to filter for DB (filter them right away).
    val outstandingSize = requiredSize - items.size

    // Collects into our list of products the attributes we care about (extract[Product]). Then filter out unwanted data.
    // fyi: throws Mapping exception.
    //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)

    val dbProducts = { if (reconWithDB) Product.getProducts  else Map[Long, Product]()}
    // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
    val productsByState: Map[EntityRecordState, IndexedSeq[ProductAsLCBOJson]] = items.groupBy {
      p => (dbProducts.get(p.id.toLong), p) match {
        case (None, _)  => New
        case (Some(product), lcboProduct) if product.isDirty(lcboProduct) => Dirty
        case (_ , _) => Clean
      }
    }

    val cleanProducts = accumItems(Clean) ++ fetchItems(Clean, productsByState, {p => p.getProduct(dbProducts)})
    val dirtyProducts = accumItems(Dirty) ++ fetchItems(Dirty, productsByState, {p => p.getProduct(dbProducts).map({ _.copyAttributes( p)})})
    val newProducts = accumItems(New) ++ productsByState.getOrElse(New, Nil).map{ p => Product.create(p) }
    val revisedAccumItems: Map[EntityRecordState, IndexedSeq[Product]] = Map(New -> newProducts, Dirty -> dirtyProducts, Clean -> cleanProducts)

    if (outstandingSize <= 0 || isFinalPage || totalPages < nextPage) {
      logger.info(uri) // log only the last one, less chatty
      return revisedAccumItems
    }
    // Deem as last page only if  LCBO tells us it's final page or we evaluate next page won't have any (when we gap due to parallelism).
    // Similarly having reached our required size,we can stop.

    // tail recursion enforced.
    collectItemsOnAPage(
      revisedAccumItems,
      urlRoot,
      outstandingSize,
      nextPage,
      myFilter,
      reconWithDB) // union of this page with next page when we are asked for a full sample
  }
}