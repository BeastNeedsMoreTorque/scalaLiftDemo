package code.model

import java.text.NumberFormat

import code.model.EntityRecordState.EnumerationValueType

import scala.collection.concurrent.TrieMap
import scala.collection._
import scala.language.implicitConversions
import scala.xml.Node
import net.liftweb.record.field.{BooleanField, IntField, LongField, StringField}
import net.liftweb.record.MetaRecord
import net.liftweb.common._
import net.liftweb.util.Props
import net.liftweb.json.Xml
import org.squeryl.annotations._

case class Attribute(key: String, value: String)

/**
  * Created by philippederome on 15-11-01. Modified 16-01-01 for Record+Squeryl (to replace Mapper), Record being open to NoSQL and Squeryl providing ORM service.
  * Product: The elements of a product from LCBO catalogue that we deem of relevant interest to replicate in DB for this toy demo.
  */
class Product private() extends Persistable[Product] with CreatedUpdated[Product] {
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

  def isDirty(p: Product): Boolean = {
    price_in_cents.get != p.price_in_cents.get ||
      image_thumb_url.get != p.image_thumb_url.get
  }
}

/**
  * object Product: supports reconcile that does insert or update on items needing updating, and refresh caches
  */
object Product extends Product with MetaRecord[Product] with Loggable {
  private val DBBatchSize = Props.getInt("product.DBBatchSize", 1)
  // thread-safe lock free objects
  private val productsCache: concurrent.Map[Long, Product] = TrieMap() // only update once confirmed in DB! Keyed by id (not lcboId)
  override val LcboIdsToDBIds: concurrent.Map[Long, Long] = TrieMap()
  override def table(): org.squeryl.Table[Product] = MainSchema.products

  def getProductByLcboId(id: Long): Option[Product] =
    for (dbId <- LcboIdsToDBIds.get(id);
         p <- productsCache.get(dbId)) yield p

  def lcboidToDBId(l: Long): Option[Long] = LcboIdsToDBIds.get(l)
  def MaxPerPage = Props.getInt("product.lcboMaxPerPage", 0)

  /* Convert a store to XML @see progscala2 chapter on implicits */
  implicit def toXml(p: Product): Node =
    <product>{Xml.toXml(p.asJValue)}</product>

  def init(): Unit = {
    logger.info("Product.init start")
    val voidLastStep: (Iterable[Product]) => Unit = {x: Iterable[Product] => Unit }
    init(voidLastStep)
    logger.info("Product.init end")
  }

  def reconcile(cacheOnly: Boolean, items: IndexedSeq[Product]): IndexedSeq[Product] = {
    // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
    val productsByState: Map[EnumerationValueType, IndexedSeq[Product]] = items.groupBy {
      p => (getProductByLcboId(p.lcboId), p) match {
        case (None, _) => EntityRecordState.New
        case (Some(product), lcboProduct) if product.isDirty(lcboProduct) => EntityRecordState.Dirty
        case (_ , _) => EntityRecordState.Clean
      }
    }
    val dirtyProducts = productsByState.getOrElse(EntityRecordState.Dirty, IndexedSeq[Product]())
    val newProducts = productsByState.getOrElse(EntityRecordState.New, IndexedSeq[Product]())
    updateAndInsert(dirtyProducts, newProducts)

    if (cacheOnly) IndexedSeq() // to satisfy method signature only
    else {
      // productsByState has dirty PKID and so need refresh prior to use in memory, which we do with flatMap calls below.
      val refreshedCleanProducts = for( prod <- productsByState.getOrElse(EntityRecordState.Clean, IndexedSeq[Product]());
                                        p <- getProductByLcboId(prod.lcboId)) yield p
      val updatedProducts = for ( prod <- dirtyProducts;
                                  p <- getProductByLcboId(prod.lcboId)) yield p
      val refreshedNewProducts = for ( prod <- newProducts;
                                  p <- getProductByLcboId(prod.lcboId)) yield p
      (refreshedCleanProducts ++ refreshedNewProducts ++ updatedProducts).toIndexedSeq // client wants full set.
    }
  }

}
