package code.model

import java.text.NumberFormat
import java.sql.SQLException

import scala.collection.concurrent.TrieMap
import scala.collection._

import net.liftweb.record.field.{LongField,StringField,BooleanField,IntField}
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.common._
import net.liftweb.util.Props
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.squerylrecord.KeyedRecord

import org.squeryl.annotations._

import MainSchema._
import code.Rest.pagerRestClient

case class Attribute(key: String, value: String)

// helper case class to extract from JSON as REST client to LCBO.
case class ProductAsLCBOJson(id: Int,
                             is_discontinued: Boolean,
                             is_dead: Boolean,
                             `package`: String,
                             total_package_units: Int,
                             primary_category: String,
                             name: String,
                             image_thumb_url: String,
                             origin: String,
                             description: String,
                             secondary_category: String,
                             serving_suggestion: String,
                             varietal: String,
                             price_in_cents: Int,
                             alcohol_content: Int,
                             volume_in_milliliters: Int) {
  def removeNulls: ProductAsLCBOJson = { // remove LCBO's poisoned null strings
    def notNull(s: String) = if (s eq null) "" else s

    ProductAsLCBOJson(id,
      is_discontinued,
      is_dead,
      notNull(`package`),
      total_package_units,
      notNull(primary_category),
      name: String,
      notNull(image_thumb_url),
      notNull(origin),
      notNull(description),
      notNull(secondary_category),
      notNull(serving_suggestion),
      notNull(varietal),
      price_in_cents,
      alcohol_content,
      volume_in_milliliters)
  }

  def getProduct(dbProducts: Map[Long, Product]) = dbProducts.get(id)

}


/**
  * Created by philippederome on 15-11-01. Modified 16-01-01 for Record+Squeryl (to replace Mapper), Record being open to NoSQL and Squeryl providing ORM service.
  * Product: The elements of a product from LCBO catalogue that we deem of relevant interest to replicate in DB for this toy demo.
  */
class Product private() extends Record[Product] with KeyedRecord[Long] with CreatedUpdated[Product]  {
  def meta = Product

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id
  val lcbo_id = new LongField(this) // we don't share same PK as LCBO!
  def lcboId = lcbo_id.get

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
  def createProductElemVals: IndexedSeq[Attribute] =
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

  def isDirty(p: ProductAsLCBOJson): Boolean = {
    price_in_cents.get != p.price_in_cents ||
      image_thumb_url.get != p.image_thumb_url
  }
  def copyAttributes(p: ProductAsLCBOJson): Product = {
    price_in_cents.set(p.price_in_cents)
    image_thumb_url.set(p.image_thumb_url)
    this
  }

}

/**
  * object Product: supports persist that does insert or update, depending whether we already have a copy of LCBO product in our database
  * Takes care of a dependency when persisting with assumption that this is always bound to a valid user request, so will attempt to store
  * to UserProducts as well.
  * Errors are possible if data is too large to fit. tryo will catch those and report them.
  */
object Product extends Product with MetaRecord[Product] with pagerRestClient with Loggable {
  private val DBBatchSize = Props.getInt("product.DBBatchSize", 1)

  // thread-safe lock free objects
  private val productsCache: concurrent.Map[Long, Product] = TrieMap() // only update once confirmed in DB! Keyed by id (not lcboId)
  private val lcboIdsToDBIds: concurrent.Map[Long, Long] = TrieMap[Long, Long]()

  def lcboidToDBId(l: Int): Option[Long] = lcboIdsToDBIds.get(l)

  def create(p: ProductAsLCBOJson): Product = {
    // store in same format as received by provider so that un-serializing if required will be same logic. This boiler-plate code seems crazy (not DRY at all)...
    createRecord.
      lcbo_id(p.id).
      name(p.name).
      primary_category(p.primary_category).
      is_discontinued(p.is_discontinued).
      `package`(p.`package`).
      origin(p.origin).
      image_thumb_url(p.image_thumb_url).
      price_in_cents(p.price_in_cents).
      total_package_units(p.total_package_units).
      volume_in_milliliters(p.volume_in_milliliters).
      alcohol_content(p.alcohol_content).
      secondary_category(p.secondary_category).
      varietal(p.varietal).
      description(p.description).
      serving_suggestion(p.serving_suggestion)
  }

  def init(): Unit = inTransaction {
    val prods = from(products)(p => select(p))
    productsCache ++= prods.map { p => p.id -> p }.toMap
    lcboIdsToDBIds ++= prods.map{ p => p.lcboId -> p.id }
  }

  def reconcile(items: IndexedSeq[ProductAsLCBOJson]) :  IndexedSeq[Product] = {
    def fetchItems(state: EntityRecordState,
                   productsByState: Map[EntityRecordState, IndexedSeq[ProductAsLCBOJson]],
                   f: ProductAsLCBOJson => Option[Product] ): IndexedSeq[Product] = {
      productsByState.getOrElse(state, Vector()).flatMap { p => f(p) } // remove the Option in Option[Product]
    }

    val prods: Map[Long, Product] = productsCache
    // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
    val productsByState: Map[EntityRecordState, IndexedSeq[ProductAsLCBOJson]] = items.groupBy {
      p => (prods.get(p.id.toLong), p) match {
        case (None, _) => New
        case (Some(product), lcboProduct) if product.isDirty(lcboProduct) => Dirty
        case (_ , _) => Clean
      }
    }

    // if there is nothing in map productsByState for New as original ProductAsLCBOJson, return empty IndexedSeq. Then take that IndexedSeq and map it to a created Product.
    // This yields indexedseq of products.
    val newProducts = productsByState.get(New).fold(IndexedSeq[ProductAsLCBOJson]()){identity}.map { Product.create }
    insertProducts(newProducts)

    val dirtyProducts = fetchItems(Dirty, productsByState, {p => p.getProduct(prods).map { _.copyAttributes( p)} })
    updateProducts(dirtyProducts)

    val cleanProducts = fetchItems(Clean, productsByState, {p => p.getProduct(prods)})

    cleanProducts ++ newProducts ++ dirtyProducts // client wants full set, meanwhile we store and cache those that represent changes.
  }

  def getProduct(dbId: Long): Option[Product] = productsCache get dbId
  def getProductByLcboId(id: Int): Option[Product] = lcboidToDBId(id).flatMap(  productsCache.get )

  // @see http://squeryl.org/occ.html
  private def updateProducts(myProducts: Seq[Product]): Unit = synchronized {
    myProducts.grouped(DBBatchSize).
      foreach { prods =>
        try {
          inTransaction { products.forceUpdate(prods) }  // @see http://squeryl.org/occ.html.
          // regular call as update throws.
          // We don't care if two threads attempt to update the same product (from two distinct stores and one is a bit more stale than the other)
          // However, there are other situations where we might well care.
        } catch {
          case se: SQLException =>
            logger.error("SQLException ")
            logger.error("Code: " + se.getErrorCode)
            logger.error("SqlState: " + se.getSQLState)
            logger.error("Error Message: " + se.getMessage)
            logger.error("NextException:" + se.getNextException)
          case e: Exception =>
            logger.error("General exception caught: " + e+ " " + prods)
        }
        // update in memory for next caller who should be blocked (updateProducts is synchronized)
        productsCache ++= prods.map { p => p.id -> p }.toMap
      }
  }

  private def insertProducts( myProducts: IndexedSeq[Product]): Unit = synchronized {
    def DBIdToLcboId(id: Long): Option[Long] = productsCache.get(id).map(_.lcboId)

    // Do special handling to filter out duplicate keys, which would throw.
    def insertBatch(filteredProds: Iterable[Product]): Unit = synchronized { // synchronize on object Product as clients are from different threads
        // insert them
      try {  // getNextException in catch is why we want to try catch here.
        // the DB could fail for PK or whatever other reason.
        inTransaction {
          products.insert(filteredProds) // refresh them with PKs assigned by DB server.
          val ids = filteredProds.map(_.lcbo_id)
          val filteredProdsWithPKs = from(products)(p => where( p.lcbo_id in ids) select(p))
          // update in memory for next caller who should be blocked (insertProducts is synchronized)
          productsCache ++= filteredProdsWithPKs.map { p => p.id -> p }.toMap
          lcboIdsToDBIds ++= filteredProdsWithPKs.map { p => p.lcboId -> p.id }
        }
      } catch {
        case se: SQLException =>
          logger.error("SQLException ")
          logger.error("Code: " + se.getErrorCode)
          logger.error("SqlState: " + se.getSQLState)
          logger.error("Error Message: " + se.getMessage)
          logger.error("NextException:" + se.getNextException)
        case e: Exception =>
          logger.error("General exception caught: " + e)
      }

    }
    // first evaluate against cache (assumed in synch with DB) what's genuinely new.
    // Then, annoying filter to ensure uniqueness by lcbo_id, take the last of each set sharing same lcbo_id and then collect the values
    val LcboIDs = productsCache.keySet.flatMap(DBIdToLcboId) // evaluate once
    val filteredForRI = myProducts.filterNot { p => LcboIDs.contains(p.lcboId) }
    val filteredForUnique = filteredForRI.groupBy {_.lcboId}.
      map { case (k,v) => v.last }
    // break it down and then serialize the work.
    filteredForUnique.grouped(DBBatchSize).foreach { insertBatch }
  }


}
