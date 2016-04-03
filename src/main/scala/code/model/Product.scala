package code.model

import java.text.NumberFormat

import scala.collection.concurrent.TrieMap
import scala.collection.{IndexedSeq, concurrent}
import scala.language.implicitConversions
import scala.xml.Node
import net.liftweb.record.field.{BooleanField, IntField, LongField, StringField}
import net.liftweb.record.MetaRecord
import net.liftweb.common._
import net.liftweb.util.Props
import net.liftweb.json._
import org.squeryl.annotations._

/**
  * Created by philippederome on 15-11-01. Modified 16-01-01 for Record+Squeryl (to replace Mapper), Record being open to NoSQL and Squeryl providing ORM service.
  * Product: The elements of a product from LCBO catalogue that we deem of relevant interest to replicate in DB for this toy demo.
  */
class Product private() extends IProduct with Persistable[Product] with Loader[Product]
  with LcboJSONExtractor[Product] with CreatedUpdated[Product] {
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

  override def MaxPerPage = Product.MaxPerPage

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
  def price: String =
    formatter format (price_in_cents.get / 100.0) // since we perverted meaning somewhat by changing unit from cents to dollars

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

}

/**
  *
  */
object Product extends Product with MetaRecord[Product] with ItemStateGrouper[Product, IProduct] with Loggable {
  // thread-safe lock free objects
  private val productsCache: concurrent.Map[Long, Product] = TrieMap() // only update once confirmed in DB! Keyed by id (not lcboId)
  def getProduct(id: Long): Option[IProduct] = productsCache.get(id)
  def getItemByLcboId(id: Long): Option[IProduct] =
    for (dbId <- LcboIdsToDBIds.get(id);
         p <- productsCache.get(dbId)) yield p

  def lcboIdToDBId(id: Long): Option[Long] = LcboIdsToDBIds.get(id)

  override val LcboIdsToDBIds: concurrent.Map[Long, Long] = TrieMap()
  override def table(): org.squeryl.Table[Product] = MainSchema.products
  override def MaxPerPage = Props.getInt("product.lcboMaxPerPage", 0)
  private val isNotDiscontinued: (Product) => Boolean = { !_.isDiscontinued }
  private val DBBatchSize = Props.getInt("product.DBBatchSize", 1)
  private val sizeFulfilled: (Int) => SizeChecker =
    requiredSize => (totalSize: Int) => requiredSize <= totalSize
  private val sizeNeverFulfilled: SizeChecker = {(totalSize: Int) => false}
  private val getCachedItem: (Product) => Option[IProduct] = {s => getItemByLcboId(s.lcboId)}

  /* Convert a store to XML @see progscala2 chapter on implicits */
  implicit def toXml(p: Product): Node =
    <product>{Xml.toXml(p.asJValue)}</product>

  def init(): Unit = {
    logger.info("Product.init start")
    load()
    logger.info("Product.init end")
  }

  /*
   * Queries LCBO matching category
   * Full URL will be built as follows: http://lcbo.com/products?store_id=<storeId>&q=<category.toLowerCase()>&per_page=<perPage>
   * LCBO allows to specify q as query to specify pattern match on product name (e.g. beer, wine)
   * for pattern match LCBO uses lower case but for actual product category it's upper case, so to make comparisons, we will need to account for that
   * primary_category in catalog or p.primary_category so we need a conversion function to adjust)
   */
  def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct] =
    collectItemsOnPages(
      s"$LcboDomainURL/products",
      Seq("store_id" -> lcboStoreId, "q" -> category),
      sizeFulfilled(requiredSize),
      isNotDiscontinued
    )

  // side effect to store updates of the products
  def fetchByStore(lcboStoreId: Long): IndexedSeq[IProduct] = {
    // by design we don't track of products by store, so this effectively forces us to fetch them from trusted source, LCBO
    // and gives us opportunity to bring our cache up to date about firm wide products.
    val items = collectItemsOnPages(
      s"$LcboDomainURL/products",
      Seq("store_id" -> lcboStoreId),
      sizeNeverFulfilled,
      isNotDiscontinued
    )
    val productsByState = itemsByState(items, getCachedItem, dirtyPredicate)
    val dirtyItems = productsByState.getOrElse(EntityRecordState.Dirty, IndexedSeq[Product]()) // unusable for cache
    val newItems = productsByState.getOrElse(EntityRecordState.New, IndexedSeq[Product]()) // unusable for cache
    updateAndInsert(dirtyItems, newItems) // updates DB AND cache.
    items.map{ _.lcboId}.flatMap{ getItemByLcboId } // usable for cache, now that we refreshed them all
  }
}
