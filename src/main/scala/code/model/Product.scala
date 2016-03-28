package code.model

import java.text.NumberFormat

import code.model.EntityRecordState.EnumerationValueType

import scala.collection.concurrent.TrieMap
import scala.collection.{IndexedSeq, concurrent}
import scala.language.implicitConversions
import scala.xml.Node
import net.liftweb.record.field.{BooleanField, IntField, LongField, StringField}
import net.liftweb.record.MetaRecord
import net.liftweb.common._
import net.liftweb.util.Props
import net.liftweb.json.Xml
import org.squeryl.annotations._

import scala.annotation.tailrec

case class Attribute(key: String, value: String)

/**
  * Created by philippederome on 15-11-01. Modified 16-01-01 for Record+Squeryl (to replace Mapper), Record being open to NoSQL and Squeryl providing ORM service.
  * Product: The elements of a product from LCBO catalogue that we deem of relevant interest to replicate in DB for this toy demo.
  */
class Product private() extends IProduct with Persistable[Product] with Loader[Product] with LcboJSONCreator[Product] with CreatedUpdated[Product] {
  def meta = Product

  @Column(name="pkid")
  override val idField = new LongField(this, 0)  // our own auto-generated id
  val lcbo_id = new LongField(this) // we don't share same PK as LCBO!

  // for Persistable
  override def table(): org.squeryl.Table[Product] = Product.table()
  override def cache() = Product.productsCache
  override def LcboIdsToDBIds() = Product.LcboIdsToDBIds
  override def pKey: Long = idField.get
  override def lcboId: Long = lcbo_id.get
  override def setLcboId(id: Long): Unit = lcbo_id.set(id)

  val is_discontinued = new BooleanField(this, false)
  val `package` = new StringField(this, 80) { // allow dropping some data in order to store/copy without SQL error (120 empirically good)
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val total_package_units = new IntField(this)
  val primary_category = new StringField(this, 40) { // allow dropping some data in order to store/copy without SQL error (120 empirically good)
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val name = new StringField(this, 120) { // allow dropping some data in order to store/copy without SQL error (120 empirically good)
    override def setFilter = notNull _ :: crop _  :: super.setFilter
  }
  val image_thumb_url = new StringField(this, 200) { // allow dropping some data in order to store/copy without SQL error (120 empirically good)
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val origin = new StringField(this, 200) { // allow dropping some data in order to store/copy without SQL error (120 empirically good)
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val price_in_cents = new IntField(this)
  val alcohol_content = new IntField(this)
  val volume_in_milliliters = new IntField(this)
  val secondary_category = new StringField(this, 80)
  val varietal = new StringField(this, 100) { // allow dropping some data in order to store/copy without SQL error (120 empirically good)
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val description = new StringField(this, 2000) {// allow dropping some data in order to store/copy without SQL error
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }
  val serving_suggestion = new StringField(this, 300) {// allow dropping some data in order to store/copy without SQL error
    override def setFilter = notNull _ :: crop _ :: super.setFilter
  }

  // intentional aliasing allowing more standard naming convention and not having to call get on.
  def Name = name.get
  def primaryCategory = primary_category.get
  def isDiscontinued = is_discontinued.get
  def totalPackageUnits = total_package_units.get
  def imageThumbUrl = image_thumb_url.get
  def Package = `package`.get // alias to avoid back ticks

  val formatter = NumberFormat.getCurrencyInstance() // Not French Canada, which does it differently...

  // Change unit of currency from cents to dollars and Int to String
  def price: String = {
    formatter format (price_in_cents.get / 100.0)
  } // since we perverted meaning somewhat by changing unit from cents to dollars

  // Change scale by 100 to normal conventions, foregoing LCBO's use of Int (for ex. 1200 becomes "12.0%" including the percent sign)
  def alcoholContent: String = {
    val a = alcohol_content.get / 100.0
    f"$a%1.1f%%"
  }

  // intentional change of scale from ml to L, using String representation instead of integer and keeping 3 decimals (0.335 ml for beer)
  def volumeInLitre: String = {
    val v = volume_in_milliliters.get / 1000.0
    f"$v%1.3f L"
  }
  /**
    *
    * @return an ordered list of pairs of values (label and value), representing most of the interesting data of the product
    */
  def streamAttributes: IndexedSeq[Attribute] =
  // order is important and would be dependent on web designer input, we could possibly find ordering rule either in database or in web design. This assumes order can be fairly static.
    ( Attribute("Name:", name.get) ::
      Attribute("Primary Category:", primary_category.get) ::
      Attribute("Secondary Category:", secondary_category.get) ::
      Attribute("Varietal:", varietal.get) ::
      Attribute ("Package:", Package) ::
      Attribute ("Volume:", volumeInLitre) ::
      Attribute ("Price:", price) ::
      Attribute("Description:", description.get) ::
      Attribute("Serving Suggestion:", serving_suggestion.get) ::
      Attribute("Alcohol content:", alcoholContent) ::
      Attribute ("Origin:", origin.get) ::
      Nil).filterNot{ attr => attr.value == "null" || attr.value.isEmpty }.toVector

  def isDirty(p: IProduct): Boolean = {
    price != p.price ||
      imageThumbUrl != p.imageThumbUrl
  }
}

/**
  *
  */
object Product extends Product with MetaRecord[Product] with Loggable {
  // thread-safe lock free objects
  private val productsCache: concurrent.Map[Long, Product] = TrieMap() // only update once confirmed in DB! Keyed by id (not lcboId)
  override val LcboIdsToDBIds: concurrent.Map[Long, Long] = TrieMap()
  override def table(): org.squeryl.Table[Product] = MainSchema.products

  /* Convert a store to XML @see progscala2 chapter on implicits */
  implicit def toXml(p: Product): Node =
    <product>{Xml.toXml(p.asJValue)}</product>

  def init(): Unit = {
    logger.info("Product.init start")
    load()
    logger.info("Product.init end")
  }

  def collectProducts(params: Seq[(String, Any)],
                      requiredSize: Int): IndexedSeq[IProduct] =
    collectItemsOnAPage(
      accumItems=IndexedSeq[Product](),
      s"$LcboDomainURL/products",
      params,
      Some(requiredSize),  // ignored if not set meaning take them all
      pageNo = 1)

  // side effect to store updates of the products
  def notifyProducts(params: Seq[(String, Any)]): IndexedSeq[IProduct] = {
    val items = collectItemsOnAPage(
      accumItems=IndexedSeq[Product](),
      s"$LcboDomainURL/products",
      params,
      None,  // ignored if not set meaning take them all
      pageNo = 1)

    // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
    val productsByState: Map[EnumerationValueType, IndexedSeq[Product]] = items.groupBy {
      p => (getProductByLcboId(p.lcboId), p) match {
        case (None, _) => EntityRecordState.New
        case (Some(product), lcboProduct) if product.isDirty(lcboProduct) => EntityRecordState.Dirty
        case (_ , _) => EntityRecordState.Clean
      }
    }
    val dirtyItems = productsByState.getOrElse(EntityRecordState.Dirty, IndexedSeq[Product]())
    val newItems = productsByState.getOrElse(EntityRecordState.New, IndexedSeq[Product]())
    updateAndInsert(dirtyItems, newItems)
    val cleanItems = productsByState.getOrElse(EntityRecordState.Clean, IndexedSeq[Product]())
    cleanItems ++ dirtyItems ++ newItems
  }

  def lcboIdToDBId(l: Long): Option[Long] = LcboIdsToDBIds.get(l)
  def getProductByLcboId(id: Long): Option[IProduct] =
    for (dbId <- LcboIdsToDBIds.get(id);
         p <- productsCache.get(dbId)) yield p

  private def MaxPerPage = Props.getInt("product.lcboMaxPerPage", 0)
  private val DBBatchSize = Props.getInt("product.DBBatchSize", 1)

  /**
    * LCBO client JSON query handler.
    *
    * @see https://github.com/lift/lift/tree/master/framework/lift-base/lift-json/
    *      don't go to more pages than user implicitly requests via requiredSize that should not be exceeded.
    *      Uses tail recursion.
    * @param accumItems accumulator to facilitate tail recursion
    * @param urlRoot a LCBO product query without the details of paging, which we handle here
    * @param requiredSize required size of products that are asked for. May get less if there are fewer matches, but will not go above that size.
    *                      if cacheOnly is true, this value can be arbitrary and ignored.
    * @param cacheOnly true if we don't need a full return result set, false if we need data we can consume (side effect is to cache all the time)
    * @param pageNo client calls this with value 1 (initial page), recursion increments it, designates the pageno for LCBO JSON data when data fits on several pages
    * @param filter client's filter that can be applied as we process the data before mapping/extracting it out to client data.
    *                 In principle, should be faster when user filters reject many values, but no empirical evidence here.
    * @return a vector of product items matching the query and size constraint (or none if cacheOnly is true). Always side effect to cache.
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
                                        params: Seq[(String, Any)],
                                        requiredSize: Option[Int],
                                        pageNo: Int): IndexedSeq[Product] = {

    val (rawItems, jsonRoot, uri) = extractLcboItems(urlRoot, params, pageNo=pageNo, MaxPerPage)
    val items = rawItems.filterNot(p => p.isDiscontinued)
    val revisedAccumItems =  accumItems ++ items

    if (isFinalPage(jsonRoot, pageNo=pageNo) ||
        requiredSize.forall{x => x <= items.size + accumItems.size }) {
      logger.info(uri) // log only last one to be less verbose
      return revisedAccumItems
    }
    collectItemsOnAPage(
      revisedAccumItems, // union of this page with next page when we are asked for a full sample
      urlRoot,
      params,
      requiredSize,
      pageNo + 1)
  }

}
