package code.model

import java.text.NumberFormat

import scala.collection.concurrent.TrieMap
import scala.collection._
import scala.language.implicitConversions
import scala.xml.Node
import net.liftweb.record.field.{BooleanField, IntField, LongField, StringField}
import net.liftweb.record.MetaRecord
import net.liftweb.util.Props
import net.liftweb.json._
import net.liftweb.util.Helpers._
import org.squeryl.annotations._
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.pageFetcher.GotEnough_?

/**
  * Created by philippederome on 15-11-01. Modified 16-01-01 for Record+Squeryl (to replace Mapper), Record being open to NoSQL and Squeryl providing ORM service.
  * Product: The elements of a product from LCBO catalogue that we deem of relevant interest to replicate in DB for this toy demo.
  */
class Product private() extends IProduct  with LCBOEntity[Product]  {
  def meta = Product

  @Column(name="pkid")
  override val idField = new LongField(this, 0)  // our own auto-generated id
  val lcbo_id = new LongField(this) // we don't share same PK as LCBO!

  // for Loader and LCBOEntity
  override def table(): org.squeryl.Table[Product] = Product.table()
  override def cache() = Product.productsCache
  override def LcboIdsToDBIds() = Product.LcboIdsToDBIds
  override def pKey: P_KEY = P_KEY(idField.get)
  override def lcboId: LCBO_ID = LCBO_ID(lcbo_id.get)
  override def setLcboId(id: LCBO_ID): Unit = lcbo_id.set(id)

  override def MaxPerPage = Product.MaxPerPage
  override def getCachedItem: (IProduct) => Option[IProduct] = s => Product.getItemByLcboId(s.lcboId)

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

  override def canEqual(other: Any) =
    other.isInstanceOf[Product]

  override def equals(other: Any): Boolean =
    other match {
      case that: Product =>
        if (this eq that) true
        else {
            that.canEqual(this) &&
            ( Name == that.Name &&
              primaryCategory == that.primaryCategory &&
              isDiscontinued == that.isDiscontinued &&
              imageThumbUrl == that.imageThumbUrl &&
              price == that.price) &&
            (alcohol_content.get == that.alcohol_content.get) // more of an exercise than anything
        }
      case _ => false
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
object Product extends Product with MetaRecord[Product] {
  // thread-safe lock free objects
  private val productsCache: concurrent.Map[P_KEY, Product] = TrieMap() // only update once confirmed in DB! Keyed by id (not lcboId)
  def getProduct(id: P_KEY): Option[IProduct] = productsCache.get(id)
  def getItemByLcboId(id: LCBO_ID): Option[IProduct] =
    for (dbId <- LcboIdsToDBIds.get(id);
         p <- productsCache.get(dbId)) yield p

  def lcboIdToDBId(id: LCBO_ID): Option[P_KEY] = LcboIdsToDBIds.get(id)

  override val LcboIdsToDBIds: concurrent.Map[LCBO_ID, P_KEY] = TrieMap()
  override def table(): org.squeryl.Table[Product] = MainSchema.products
  override def MaxPerPage = Props.getInt("product.lcboMaxPerPage", 0)
  private val sizeFulfilled: (Int) => GotEnough_? =
    requiredSize => (totalSize: Int) => requiredSize <= totalSize
  override def getCachedItem: (IProduct) => Option[IProduct] = s => getItemByLcboId(s.lcboId)

  val queryByCategoryArgs = getSeq("product.query.ByCategoryArgs")
  val queryAllItemsArgs = getSeq("product.query.AllItemsArgs")

  /* Convert a store to XML @see progscala2 chapter on implicits or Scala in Depth implicit view */
  implicit def toXml(p: Product): Node =
    <product>{Xml.toXml(p.asJValue)}</product>

  // the implicit isEnough parameter is strictly to play around with the concept as in this case, implicit is not particularly compelling.
  // See the calls to productWebQuery and collectItemsAsWebClient. Though, one might argue choosing single pages,n pages, or all pages could represent
  // a cross cutting concern or a strategy.
  private def productWebQuery(lcboStoreId: Long, seq: Seq[(String, Any)])( implicit isEnough: GotEnough_? = pageFetcher.neverEnough ) =
    collectItemsAsWebClient(s"$LcboDomainURL/products", LcboExtract, ("store_id" -> lcboStoreId) +: seq)

  /*
   * Queries LCBO matching category
   * URL will be built as follows: http://lcbo.com/products?store_id=<storeId>&q=<category.toLowerCase()>&per_page=<perPage> (+ details on where_not and order)
   * LCBO allows to specify q as query to specify pattern match on product name (e.g. beer, wine)
   */
  def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct] = {
    implicit val checkUpToRequiredSize = sizeFulfilled(requiredSize)
    // don't fetch more than required size.
    productWebQuery(lcboStoreId, Seq("q" -> category) ++ queryByCategoryArgs)
  }

  // side effect to store updates of the products
  def fetchByStore(lcboStoreId: Long): IndexedSeq[IProduct] = {
    val box = tryo {
      // by design we don't track of products by store, so this effectively forces us to fetch them from trusted source, LCBO
      // and gives us opportunity to bring our cache up to date about firm wide products.
      val prods = productWebQuery( lcboStoreId, queryAllItemsArgs)
      synchDirtyAndNewItems(prods, getCachedItem, dissimilar) // the side effect
      prods.map{ _.lcboId}.flatMap{ getItemByLcboId } // usable for cache, now that we refreshed them all
    }
    val fullContextErr = { (m: String, err: String) =>
      s"Problem loading products into cache with message $m and exception error $err"
    }
    val (check, err) = checkErrors(box, fullContextErr, simpleErr = "Problem loading products into cache" )
    if (!check) logger.error(err)
    box.toOption.fold(IndexedSeq[IProduct]()){ identity }
  }
}
