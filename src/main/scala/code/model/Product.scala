package code.model

import java.text.NumberFormat
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import net.liftweb.json._
import net.liftweb.record.MetaRecord
import net.liftweb.record.field.{BooleanField, IntField, LongField}
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import org.squeryl.Table
import org.squeryl.annotations._
import scala.collection.concurrent.TrieMap
import scala.collection.{Seq, _}
import scala.language.implicitConversions
import scala.util.Try
import scala.xml.Node

/**
  * Created by philippederome on 15-11-01. Modified 16-01-01 for Record+Squeryl (to replace Mapper),
  * Record being open to NoSQL and Squeryl providing ORM service.
  * Product: The elements of a product from LCBO catalogue that we deem of relevant interest to replicate in DB for this toy demo.
  */
class Product private() extends LCBOEntity[Product] with IProduct {
  @Column(name="pkid")
  override val idField = new LongField(this, 0)  // our own auto-generated id
  val lcbo_id = new LongField(this) // we don't share same PK as LCBO!
  val is_discontinued = new BooleanField(this, false)
  val `package` = new FilteredOptionalStringField(80)
  val total_package_units = new IntField(this)
  val primary_category = new FilteredMandatoryStringField(40)
  val name = new FilteredMandatoryStringField(120)

  val image_thumb_url = new FilteredOptionalStringField(200)
  val origin = new FilteredOptionalStringField(200)
  val price_in_cents = new IntField(this)
  val alcohol_content = new IntField(this)
  val volume_in_milliliters = new IntField(this)
  val secondary_category = new FilteredMandatoryStringField(80)
  val varietal = new FilteredOptionalStringField(100)
  val description = new FilteredOptionalStringField(2000)
  val serving_suggestion = new FilteredOptionalStringField(300)
  val formatter = NumberFormat.getCurrencyInstance() // Not French Canada, which does it differently...

  def meta: MetaRecord[Product] = Product

  // for Loader and LCBOEntity
  override def table: Table[Product] = Product.table

  override def cache: concurrent.Map[P_KEY, Product] = Product.cache

  override def lcboIdToPK: concurrent.Map[LCBO_ID, P_KEY] = Product.lcboIdToPKMap

  override def pKey: P_KEY = P_KEY(idField.get)

  override def lcboId: LCBO_ID = LCBO_ID(lcbo_id.get)

  def imageThumbUrl: String = image_thumb_url.getValue
  def Name: String = name.get

  def totalPackageUnits: Int = total_package_units.get

  override def equals(other: Any): Boolean =
    other match {
      case that: Product =>
        (this eq that) ||
        (that.canEqual(this) &&
          name == that.name &&
          primaryCategory == that.primaryCategory &&
          isDiscontinued == that.isDiscontinued &&
          imageThumbUrl == that.imageThumbUrl &&
          price == that.price &&
          alcohol_content.get == that.alcohol_content.get) // more of an exercise than anything
      case _ => false
    }

  def primaryCategory: String = primary_category.get

  def isDiscontinued: Boolean = is_discontinued.get

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Product]

  /**
    *
    * @return an ordered list of pairs of values (label and value), representing most of the interesting data of the product
    */
  def streamAttributes: IndexedSeq[Attribute] =
  // order is important and would be dependent on web designer input, we could possibly find ordering rule either in database
  // or in web design. This assumes order can be fairly static.
    ( Attribute("Name:", name.get) ::
      Attribute("Primary Category:", primary_category.get) ::
      Attribute("Secondary Category:", secondary_category.get) ::
      Attribute("Varietal:", varietal.getValue) ::
      Attribute("Package:", `package`.getValue) ::
      Attribute("Volume:", volumeInLitre) ::
      Attribute("Price:", price) ::
      Attribute("Description:", description.getValue) ::
      Attribute("Serving Suggestion:", serving_suggestion.getValue) ::
      Attribute("Alcohol content:", alcoholContent) ::
      Attribute ("Origin:", origin.getValue) ::
      Nil).filterNot{ attr => attr.value == "null" || attr.value.isEmpty }.toVector

  // Change unit of currency from cents to dollars and Int to String
  def price: String =
    formatter.format(price_in_cents.get / 100.0) // since we perverted meaning somewhat by changing unit from cents to dollars

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
}

/**
  *
  */
object Product extends Product with MetaRecord[Product] with ProductRunner  {
  override type GotEnough_? = Int => Boolean

  // thread-safe lock free objects
  override val cache: concurrent.Map[P_KEY, Product] = TrieMap() // only update once confirmed in DB!
  val lcboIdToPKMap: concurrent.Map[LCBO_ID, P_KEY] = TrieMap()
  val MaxPerPage = Props.getInt("product.lcboMaxPerPage", 0)
  val queryByCategoryArgs = getSeq("product.query.ByCategoryArgs")(ConfigPairsRepo.defaultInstance)
  // seems difficult to apply D.I. here, so access global object.
  val queryFilterArgs = getSeq("product.query.Filter")(ConfigPairsRepo.defaultInstance)
  protected val sizeFulfilled: Int => GotEnough_? =
    requiredSize => (totalSize: Int) => requiredSize <= totalSize

  def getProduct(id: P_KEY): Option[IProduct] = cache.get(id)

  override def table: Table[Product] = MainSchema.products

  /* Convert a store to XML @see progscala2 chapter on implicits or Scala in Depth implicit view */
  implicit def toXml(p: Product): Node =
    <product>{Xml.toXml(p.asJValue)}</product>

  /*
  * Queries LCBO matching category
  * URL will be built as follows: http://lcbo.com/products?store_id=<storeId>&q=<category.toLowerCase()>&per_page=<perPage> (+ details on where_not and order)
  * LCBO allows to specify q as query to specify pattern match on product name (e.g. beer, wine)
  */
  override def fetchByStoreCategory(lcboStoreId: Long, category: String, requiredSize: Int): IndexedSeq[IProduct] = {
    implicit val checkUpToRequiredSize = sizeFulfilled(requiredSize)
    // don't fetch more than required size.
    productWebQuery(lcboStoreId, Seq("q" -> category) ++ queryByCategoryArgs)
  }

  // side effect to store updates of the products
  def fetchByStore(lcboStoreId: Long): Try[IndexedSeq[IProduct]] = Try {
    // by design we don't track of products by store, so this effectively forces us to fetch them from trusted source, LCBO
    // and gives us opportunity to bring our cache up to date about firm wide products.
    val prods = productWebQuery( lcboStoreId, queryFilterArgs) // take them all from Stream
    synchDirtyAndNewItems(prods, getCachedItem) // the side effect
    prods.map{ _.lcboId}.flatMap{ getItemByLcboId } // usable for client to cache, now that we refreshed them all
  }

  def getItemByLcboId(id: LCBO_ID): Option[IProduct] =
    for (dbId <- lcboIdToPKMap.get(id);
         p <- cache.get(dbId)) yield p

  def getCachedItem: IProduct => Option[IProduct] = s => getItemByLcboId(s.lcboId)

  // the implicit isEnough parameter is strictly to play around with the concept as in this case, implicit is not particularly compelling.
  // See the calls to productWebQuery and collectItemsAsWebClient. Though, one might argue choosing single pages,n pages, or all pages could represent
  // a cross cutting concern or a strategy.
  protected def productWebQuery(lcboStoreId: Long, seq: Seq[(String, Any)])( implicit isEnough: GotEnough_? = neverEnough ) =
    collectItemsAsWebClient("/products", extract, Seq("per_page" -> MaxPerPage, "store_id" -> lcboStoreId) ++ seq)
}
