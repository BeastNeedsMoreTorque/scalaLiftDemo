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
import net.liftweb.json.JsonAST

import org.squeryl.annotations._

import MainSchema._
import code.Rest.pagerRestClient

case class Attribute(key: String, value: String)

/**
  * Created by philippederome on 15-11-01. Modified 16-01-01 for Record+Squeryl (to replace Mapper), Record being open to NoSQL and Squeryl providing ORM service.
  * Product: The elements of a product from LCBO catalogue that we deem of relevant interest to replicate in DB for this toy demo.
  */
class Product private() extends Record[Product] with KeyedRecord[Long] with CreatedUpdated[Product]  {
  def meta = Product

  @Column(name="pkid")
  override val idField = new LongField(this, 0)  // our own auto-generated id
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

  def isDirty(p: Product): Boolean = {
    price_in_cents.get != p.price_in_cents.get ||
      image_thumb_url.get != p.image_thumb_url.get
  }
}

/**
  * object Product: supports reconcile that does insert or update on items needing updating, and refresh caches
  * supports JSON parsing a list of LCBO products with extractFromJValueSeq, which has a peculiarity to deal with special "id" field from LCBO.
  */
object Product extends Product with MetaRecord[Product] with pagerRestClient with Loggable {
  private val DBBatchSize = Props.getInt("product.DBBatchSize", 1)
  private implicit val formats = net.liftweb.json.DefaultFormats

  // thread-safe lock free objects
  private val productsCache: concurrent.Map[Long, Product] = TrieMap() // only update once confirmed in DB! Keyed by id (not lcboId)
  private val LcboIdsToDBIds: concurrent.Map[Long, Long] = TrieMap[Long, Long]()

  def getProduct(dbId: Long): Option[Product] = productsCache get dbId
  def getProductByLcboId(id: Long): Option[Product] = lcboidToDBId(id).flatMap(  productsCache.get )
  def lcboidToDBId(l: Long): Option[Long] = LcboIdsToDBIds.get(l)

  def init(): Unit = {
    logger.info(s"Product.init start") // potentially slow if products select is big
    inTransaction {
      val prods = from(products)(p => select(p))
      addNewProductsToCaches(prods)
    }
    logger.info(s"Product.init end")
  }

  def extractFromJValueSeq(itemNodes: IndexedSeq[JsonAST.JValue]): IndexedSeq[Product] = {
    import scala.collection.mutable.ArrayBuffer
    var items = ArrayBuffer[Product]()
    for (p <- itemNodes) {
      val key = (p \ "id").extractOrElse[Int](0)
      if (key > 0) {
        var item: Product = Product.createRecord
        item.lcbo_id.set(key) //hack. Record is forced to use "id" as read-only def... Because of PK considerations at Squeryl.
        setFieldsFromJValue(item, p)
        items += item
      }
    }
    items.toIndexedSeq
  }

  def reconcile(items: IndexedSeq[Product]) :  IndexedSeq[Product] = {
    val prods = productsCache.toMap
    // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
    val productsByState: Map[EntityRecordState, IndexedSeq[Product]] = items.groupBy {
      p => (prods.get(p.id), p) match {
        case (None, _) => New
        case (Some(product), lcboProduct) if product.isDirty(lcboProduct) => Dirty
        case (_ , _) => Clean
      }
    }
    val cleanProducts = productsByState.getOrElse(Clean, IndexedSeq[Product]()).
      flatMap{p => lcboidToDBId(p.lcbo_id.get).flatMap { productsCache.get} }
    val newProducts = productsByState.getOrElse(New, IndexedSeq[Product]())
    insertProducts(newProducts)

    val dirtyProducts = productsByState.getOrElse(Dirty, IndexedSeq[Product]()).
      flatMap{p => lcboidToDBId(p.lcbo_id.get).flatMap { productsCache.get} }
    updateProducts(dirtyProducts)

    cleanProducts ++ newProducts ++ dirtyProducts // client wants full set, meanwhile we store and cache those that represent changes from what we knew.
  }

  private def addNewProductsToCaches(prods: Iterable[Product]): Unit = {
    productsCache ++= prods.map { p => p.id -> p }(breakOut)
    LcboIdsToDBIds ++= productsCache.map { case(k,v) => v.lcboId -> k }
  }

  // @see http://squeryl.org/occ.html
  private def updateProducts(myProducts: Seq[Product]): Unit = {
    myProducts.grouped(DBBatchSize).
      foreach { prods =>
        try {
          inTransaction { products.forceUpdate(prods) }  // @see http://squeryl.org/occ.html.
          // regular call as update throws.
          // We don't care if two threads attempt to update the same product (from two distinct stores and one is a bit more stale than the other)
          // However, there are other situations where we might well care.
        } catch {
          case se: SQLException =>
            logger.error(s"SQLException $prods")
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

  private def insertProducts( myProducts: IndexedSeq[Product]): Unit = {
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
          addNewProductsToCaches(filteredProdsWithPKs)
        }
      } catch {
        case se: SQLException =>
          logger.error(s"SQLException $filteredProds")
          logger.error("Code: " + se.getErrorCode)
          logger.error("SqlState: " + se.getSQLState)
          logger.error("Error Message: " + se.getMessage)
          logger.error("NextException:" + se.getNextException)
        case e: Exception =>
          logger.error("General exception caught: " + e)
      }

    }
    // first evaluate against cache (assumed in synch with DB) what's genuinely new.
    val LcboIDs = productsCache.map{ case (id, p) => p.lcboId}.toSet // evaluate once
    val filteredForRI = myProducts.filterNot { p => LcboIDs.contains(p.lcboId) }
    // you never know... Our input could have the same product twice in the collection with the same lcbo_id and we have unique index in DB against that.
    val filteredForUnique = filteredForRI.groupBy {_.lcboId}.
      map { case (k,v) => v.last }
    // break it down and then serialize the work.
    filteredForUnique.grouped(DBBatchSize).foreach { insertBatch }
  }
}
