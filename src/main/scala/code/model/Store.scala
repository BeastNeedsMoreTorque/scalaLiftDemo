package code.model

import java.io.IOException
import java.net.SocketTimeoutException
import java.sql.SQLException

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

class Store private() extends Record[Store] with KeyedRecord[Long] with CreatedUpdated[Store] with Loggable  {
  private implicit val formats = net.liftweb.json.DefaultFormats

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

  //products is a ManyToMany[Product,Inventory], it extends Query[Product]
  lazy val storeProducts = MainSchema.inventories.leftStateful(this)

  def meta = Store

  def copyAttributes(my: Store, s: PlainStoreAsLCBOJson): Store = {
    my.is_dead.set(s.is_dead)
    my.address_line_1.set(s.address_line_1)
    my
  }

  def dirty_?(s: PlainStoreAsLCBOJson): Boolean = {
    is_dead.get != s.is_dead ||
      address_line_1.get != s.address_line_1
  }

  private def productsById: Map[Long, Product] = storeProducts.toIndexedSeq.groupBy(_.id).mapValues(_.head)
  private def productsByLcboId: Map[Long, Product] = storeProducts.toIndexedSeq.groupBy(_.lcbo_id.get).mapValues(_.head)

  private def productsByCategory: Map[String, IndexedSeq[Product]] = storeProducts.toIndexedSeq.groupBy(_.primaryCategory)
  private def inventories = storeProducts.associations

  private def inventoriesPerProductInStore: Map[Long, Inventory] =
    inventories.toIndexedSeq.map { inv: Inventory => inv.productid -> inv } (breakOut)

  private def emptyInventory: Boolean =
    inventories.toIndexedSeq.forall(_.quantity == 0)

  private def getInventory(prodId: Long): Option[Inventory] =
    inventoriesPerProductInStore.get(prodId)

  private def getInventoryQuantity(prodId: Long): Option[Long] =
    inventoriesPerProductInStore.get(prodId).map( _.quantity)

  private def getProductIdsByStoreCategory(category: String): IndexedSeq[Long] =
    storeProducts.toIndexedSeq.filter(_.primaryCategory == category).map(_.id)

  private def hasProductsByStoreCategory(category: String): Boolean =
    storeProducts.exists{ _.primaryCategory == category }

  private def getInventories(dbStoreId: Long): Map[Long, Inventory] =
    inventories.toSeq.map (inv => inv.productid -> inv ) (breakOut)

  /**
    * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
    * Select a random product that matches the parameters subject to a max sample size.
    *
    * @param store a String representing a numeric code of a LCBO store
    * @param category a String such as beer, wine, mostly matching primary_category at LCBO, or an asset category.
    * @return
    */
  def recommend(category: String, requestSize: Int): Box[Iterable[(Long, Product)]] = {
    tryo {
      // we could get errors going to LCBO, this tryo captures those.
      if (hasProductsByStoreCategory(category)) {
        // get some random sampling.
        val prodIds = Random.shuffle(getProductIdsByStoreCategory(category))
        // generate double the keys and hope it's enough to have nearly no 0 inventory as a result
        val seq = for (id <- prodIds;
                       p <- Product.getProduct(id)) yield p
        // generate double the keys and hope it's enough to find enough products with positive inventory as a result
        // checking quantity in for comprehension above is cost prohibitive.
        seq.take(2 * requestSize).map { p: Product =>
          (getInventoryQuantity(p.id).fold(0.toLong) {
            identity}, p)
        }.filter { _._1 > 0 }.take(requestSize)
      }
      else {
        logger.warn(s"recommend cache miss for $id") // don't try to load it asynchronously as that's start up job to get going with it.
        val prods = productsByStoreCategory(category) // take a hit of one go to LCBO, no more.
        asyncLoadCache()
        val indices = Random.shuffle[Int, IndexedSeq](prods.indices)
        val seq =
          // just use 0 as placeholder for inventory for now as we don't have this info yet.
          for (idx <- indices;
               lcbo_id = prods(idx).lcboId.toInt;
               prod <- Product.getProductByLcboId(lcbo_id)) yield (0.toLong, prod)

        // we may have 0 inventory, browser should try to finish off that work not web server.
        seq.take(requestSize)
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
  private final def collectItemsOnAPage(accumItems: IndexedSeq[Product],
                                        urlRoot: String,
                                        requiredSize: Int,
                                        pageNo: Int,
                                        myFilter: ProductAsLCBOJson => Boolean = { p: ProductAsLCBOJson => true }): IndexedSeq[Product] = {

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

    val revisedAccumItems =  accumItems ++ Product.reconcile(items)

    if (outstandingSize <= 0 || isFinalPage || totalPages < pageNo + 1) {
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
      pageNo + 1,
      myFilter) // union of this page with next page when we are asked for a full sample
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
  private def productsByStoreCategory(category: String): IndexedSeq[Product] = {
    if (Store.MaxSampleSize <= 0) Vector()
    else {
      val url = s"$LcboDomainURL/products?store_id=${lcboId}" + additionalParam("q", category) // does not handle first one such as storeId, which is artificially mandatory
      val filter = { p: ProductAsLCBOJson => p.primary_category == LiquorCategory.toPrimaryCategory(category) &&
        !p.is_discontinued
      } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers
      collectItemsOnAPage(
        IndexedSeq[Product](),
        url,
        Store.MaxSampleSize,
        pageNo = 1)
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
  private def fetchProductsByStore(): Unit = {
    if (Store.productCacheSize > 0) {
      val initSeq = IndexedSeq[Product]()

      val url = s"$LcboDomainURL/products?store_id=$lcboId"
      val filter = { p: ProductAsLCBOJson => !p.is_discontinued } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers

      collectItemsOnAPage(
        initSeq,
        url,
        Store.productCacheSize,
        1, // programmer client is assumed to know we use this as a page.
        filter).take(Store.productCacheSize)
    }
  }

  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  private def fetchInventoriesByStore(): Unit = {
    @throws(classOf[net.liftweb.json.MappingException])
    @throws(classOf[net.liftweb.json.JsonParser.ParseException])
    @throws(classOf[java.io.IOException])
    @throws(classOf[java.net.SocketTimeoutException])
    @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
    @tailrec
    def collectInventoriesOnAPage(  urlRoot: String,
                                    pageNo: Int): Unit = {
      def fetchItems(state: EntityRecordState,
                     mapVector: Map[EntityRecordState, IndexedSeq[InventoryAsLCBOJson]],
                     f: InventoryAsLCBOJson => Option[Inventory] ): IndexedSeq[Inventory] = {
        mapVector.getOrElse(state, Vector()).flatMap { p => f(p) } // remove the Option in Option[Product]
      }

      def stateOfProduct(item: InventoryAsLCBOJson): EntityRecordState = {
        val invOption =
          for (dbProductId <- lcboidToDBId(item.product_id);
               i <- getInventory(dbProductId)) yield i
        invOption  match {
          case None  => New
          case Some(inv) if inv.dirty_?(item) => Dirty
          case _ => Clean
        }
      }

      // specify the URI for the LCBO api url for liquor selection
      val uri = urlRoot + additionalParam("per_page", MaxPerPage) + additionalParam("page", pageNo) // get as many as possible on a page because we could have few matches.
      val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
      val jsonRoot = parse(pageContent) // fyi: throws ParseException
      val itemNodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
      val filter = { p: InventoryAsLCBOJson => !p.is_dead } // filter accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers
      val items = (for (p <- itemNodes) yield p.extract[InventoryAsLCBOJson].removeNulls).filter(filter)  // LCBO sends us poisoned useless nulls that we need to filter for DB (filter them right away).
      // Collects into our list of inventories the attributes we care about (extract[InventoryAsLCBOJson]). Then filter out unwanted data.

      // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
      storeProducts.refresh // make sure we're synched up with DB for the store.
      val storeProductsByState: Map[EntityRecordState, IndexedSeq[InventoryAsLCBOJson]] = items.toIndexedSeq.groupBy( stateOfProduct )

      val storeInventories = getInventories(id)
      def inventoryForStoreAndLCBOProd(lcboProdID: Int): Option[Inventory] = {
        val dbProdId = Product.lcboidToDBId(lcboProdID )
        dbProdId.map{ id: Long => storeInventories.get(id) }.getOrElse(None)
      }

      val dirtyInventories = fetchItems(Dirty, storeProductsByState, {inv => inventoryForStoreAndLCBOProd(inv.product_id).map { _.copyAttributes( inv)} } )
      // get the New partition, returning Nil if we don't have any, and open up option for a match on the productID in database from the LCBO ID.
      // finally fetch a Inventory that is created with specified productId, quantity, and storeId.
      val newInventories = storeProductsByState.getOrElse(New, Nil).
        flatMap { inv => lcboidToDBId(inv.product_id).
          map { dbProductId: Long => new Inventory(id, dbProductId, inv.quantity ) } } (collection.breakOut)

      // God forbid, we might supply ourselves data that violates composite key. Weed it out!
      def removeCompositeKeyDupes(invs: IndexedSeq[Inventory]): Iterable[Inventory] = {
        invs.groupBy(x => (x.productid, x.storeid)).map{case (k,v) => v.last}
      }
      val CompKeyFilterNewInventories = removeCompositeKeyDupes(newInventories)
      val CompKeyFilterDirtyInventories = removeCompositeKeyDupes(dirtyInventories)

      try {  // getNextException in catch is why we want to try catch here.
        inTransaction {
          // we refresh just before splitting the inventories into clean, dirty, new classes.
          MainSchema.inventories.insert(CompKeyFilterNewInventories)
          MainSchema.inventories.update(CompKeyFilterDirtyInventories)
        }
      } catch {
          case se: SQLException =>  // the bad
            logger.error(s"SQLException New Invs $CompKeyFilterNewInventories Dirty Invs $CompKeyFilterDirtyInventories")
            logger.error("Code: " + se.getErrorCode)
            logger.error("SqlState: " + se.getSQLState)
            logger.error("Error Message: " + se.getMessage)
            logger.error("NextException:" + se.getNextException)  // the "good".
          case e: Exception =>  // the UGLY!
            logger.error("General exception caught: " + e)
        }

      //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
      val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
      val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)
      if (isFinalPage || totalPages < pageNo +1) {
        logger.info(uri) // log only last one to be less verbose
        return
      }
      collectInventoriesOnAPage( urlRoot, pageNo + 1) // union of this page with next page when we are asked for a full sample
    }
    collectInventoriesOnAPage(s"$LcboDomainURL/inventories?store_id=${lcboId}", 1) // programmer client is assumed to know we use this as a page.
  }

  // may have side effect to update database with more up to date from LCBO's content (if different)
  private def loadCache(): Unit = {
    def fetchProducts(): Unit = {
      inTransaction {
        tryo { fetchProductsByStore() } match {
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading products into cache for '${lcboId}' with message $m and exception error $ex")
          case Empty =>
            logger.error(s"Problem loading products into cache for '${lcboId}'")
          case _ => ;
        }
      }
    }

    def fetchInventories(): Unit = {
      inTransaction {
        tryo { fetchInventoriesByStore() } match {
          case net.liftweb.common.Failure(m, ex, _) =>
            logger.error(s"Problem loading inventories into cache for '${lcboId}' with message $m and exception error $ex")
          case Empty =>
            logger.error(s"Problem loading inventories into cache for '${lcboId}'")
          case _ => ;
        }
      }
    }

    logger.info(s"loadCache start ${lcboId}")
    // fetch and then make sure model/Squeryl classes update to DB and their cache synchronously, so we can use their caches.
    fetchProducts() // updates products on each query if something new comes up.
    fetchInventories() // and similarly for inventories
    logger.info(s"loadCache ended ${lcboId}") // 30 seconds from last LCBO query to completing the cache update (Jan 23). Needs to be better.
  }

  def asyncLoadCache(): Unit = {
    // A kind of guard: Two piggy-backed requests to loadCache for same store will thus ignore second one.
    // Slightly unwanted consequence is that clients need to test for empty set and not assume it's non empty.
    // we impose referential integrity so we MUST get products and build on that to get inventories that refer to products
    // Note: we may execute this function, get nothing back from LCBO (e.g. website down) and still provide user data because of our db store.
    if (Store.storeProductsLoaded.putIfAbsent(id, Unit).isEmpty) {
      val fut = Future { loadCache() }
      fut foreach {
        case m =>
          inTransaction {
            // we're in async callback, need to acquire a valid session for our refresh and our query to DB on store's inventory.
            storeProducts.refresh  // key for whole inventory caching to work! We've persisted along the way for each LCBO page
            logger.debug(s"loadCache succeeded for ${lcboId}")
            if (emptyInventory) {
              logger.warn(s"got NO product inventory for storeId ${lcboId} !") // No provision for retrying.
            }
          }
      }
      fut.failed foreach {
        case f =>
          logger.info(s"loadCache explicitly failed for ${lcboId} cause $f")
      }
    }
  }

}

object Store extends Store with MetaRecord[Store] with pagerRestClient with Loggable {
  private implicit val formats = net.liftweb.json.DefaultFormats
  private val MaxSampleSize = Props.getInt("store.maxSampleSize", 0)
  private val DBBatchSize = Props.getInt("store.DBBatchSize", 1)

  private val storeLoadBatchSize = Props.getInt("store.loadBatchSize", 10)

  private val productCacheSize = Props.getInt("product.cacheSize", 10000)
  private val synchEmptiesFirst = Props.getBool("store.synchEmptiesFirst", false)

  private val storesCache: concurrent.Map[Long, Store] = TrieMap[Long, Store]()  // primary cache
  private val LcboIdsToDBIds: concurrent.Map[Long, Long] = TrieMap[Long, Long]() //secondary dependent cache

  private val storeProductsLoaded: concurrent.Map[Long, Unit] = TrieMap() // auxilliary independent cache
  // effectively a thread-safe lock-free set, which helps avoiding making repeated requests for cache warm up for a store.

  def availableStores: Set[Long] = storesCache.toMap.keySet
  def lcboIdToDBId(l: Int): Option[Long] = LcboIdsToDBIds.get(l)
  def storeIdToLcboId(s: Long): Option[Long] = storesCache.get(s).map(_.lcboId)
  def getStoreById(s: Long): Option[Store] = storesCache.get(s)

  override def MaxPerPage = Props.getInt("store.lcboMaxPerPage", 0)
  override def MinPerPage = Props.getInt("store.lcboMinPerPage", 0)

    /* Convert a store to XML */
  implicit def toXml(st: Store): Node =
    <store>{Xml.toXml(st.asJValue)}</store>

  @volatile
  var dummy: Any = _
  def timed[T](body: =>T): Double = {
    val start = System.nanoTime
    dummy = body
    val end = System.nanoTime
    ((end - start) / 1000) / 1000.0
  }


  private def getStores(dbStores: Map[Long, Store]): Unit = {
    def synchronizeData(): Box[Unit] = {
      def fetchItems(state: EntityRecordState,
                     storesByState: Map[EntityRecordState, IndexedSeq[PlainStoreAsLCBOJson]],
                     f: PlainStoreAsLCBOJson => Option[Store] ): IndexedSeq[Store] =
        storesByState.getOrElse(state, Vector()).flatMap{ f(_)}

      // collects stores individually from LCBO REST as PlainStoreAsLCBOJson on all pages starting from a pageNo.
      // we declare types fairly often in the following because it's not trivial to follow otherwise
      @tailrec
      def collectStoresOnAPage(urlRoot: String,
                               pageNo: Int): Unit = {
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
            case (Some(store), lcboStore) if store.dirty_?(lcboStore) => Dirty
            case (_ , _) => Clean  // or decided not to handle such as stores "out of bound" that we won't cache.
          }
        }

        val dirtyStores = fetchItems(Dirty, storesByState, s => s.getStore(dbStores).map(copyAttributes(_, s) ))
        val newStores = storesByState.getOrElse(New, IndexedSeq[PlainStoreAsLCBOJson]()).
          map{ create }

        def removeDupeKeys(seq: IndexedSeq[Store]): Iterable[Store] = { // you never know what bad inputs you might get.
          seq.groupBy(s => s.lcboId).map{case (k,v) => v.last}
        }
        val filteredNewStores = removeDupeKeys(newStores)
        val filteredDirtyStores = removeDupeKeys(dirtyStores)

        // identify the dirty and new stores for batch update and then retain all of them as the requested set
        inTransaction {
          // for the activity on separate thread for synchronizeData
          // batch update the database now.
          try { // getNextException is actually very useful, which is why we do try catch here
            updateStores(filteredDirtyStores)
            insertStores(filteredNewStores)
          } catch {
            case se: SQLException =>
              logger.error(s"SQLException on uri $uri dirty stores: $filteredDirtyStores new stores: $filteredNewStores")
              logger.error("Code: " + se.getErrorCode)
              logger.error("SqlState: " + se.getSQLState)
              logger.error("Error Message: " + se.getMessage)
              logger.error("NextException:" + se.getNextException)
            case e: Exception =>
              logger.error("General exception caught: " + e)
          }
        }
        val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)

        if (pageStores.isEmpty || isFinalPage) {
          logger.info(uri) // log only last one to be less verbose
          return // no need to look at more pages
        }
        collectStoresOnAPage(urlRoot, pageNo + 1) // union of this page with next page when we are asked for a full sample
      }

      // we'd like the is_dead ones as well to update state (but apparently you have to query for it explicitly!?!?)
      val url = s"$LcboDomainURL/stores?"
      tryo {
        // gather stores on this page (url) with classification as to whether they are new, dirty or clean
        collectStoresOnAPage(url, 1)
        logger.debug(s"done loading stores from LCBO")
      }
    }

    synchronizeData() match {
      case Full(m) => ;
      case net.liftweb.common.Failure(m, ex, _) =>
        logger.error(s"Problem loading LCBO stores into cache with message '$m' and exception error '$ex'")
      case Empty =>
        logger.error(s"Problem loading LCBO stores into cache, none found")
    }
  }

  private def addNewStoresToCaches(seq: Iterable[Store]): Unit = {
    storesCache ++= seq.map(s => s.id -> s)(breakOut)
    LcboIdsToDBIds ++= storesCache.map { case(k,v) => v.lcboId -> k }
  }

  def init() = {
    logger.info(s"Store.init start")
    // load all stores from DB for navigation and synch with LCBO for possible delta (small set so we can afford synching, plus it's done async way)
    val dbStores = inTransaction { from(stores)(s => select (s)) }
    addNewStoresToCaches(dbStores)
    asyncGetStores(dbStores.map {s => s.id -> s }(breakOut))
  }

  def asyncGetStores(x: Map[Long, Store]): Unit = {
    val fut = Future { getStores(x) }
    fut foreach {
      case m =>
        logger.info(s"asyncGetStores (asynch for Store.init) completed")
    }
    fut.failed foreach {
      case f =>
        logger.error(s"asyncGetStores explicitly failed with cause $f") // pretty fatal at this point.
    }
  }

  private def create(s: PlainStoreAsLCBOJson): Store = {
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

  def findAllAsLCBO(): Iterable[StoreAsLCBOJson] =
    storesCache.values.map(StoreAsLCBOJson(_))

  def findAll(): Iterable[Store] =
    storesCache.values


  private def updateStores(myStores: Iterable[Store]) = {
    myStores.grouped(DBBatchSize).
      foreach { stores.update }
    storesCache ++= myStores.map{s => s.id -> s} (breakOut)
  }

  private def insertStores( newStores: Iterable[Store]) = {
    newStores.grouped(DBBatchSize).
      foreach { stores.insert }
    addNewStoresToCaches(newStores)
  }

}