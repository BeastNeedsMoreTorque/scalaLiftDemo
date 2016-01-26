package code.model

import java.io.IOException
import java.net.SocketTimeoutException

import code.Rest.pagerRestClient
import code.model.MainSchema._
import net.liftweb.common.Loggable
import net.liftweb.record.field.{LongField,IntField}
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.util.Props
import org.squeryl.annotations.Column

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.collection.{concurrent, Map, Iterable}
import scala.language.implicitConversions


case class InventoryAsLCBOJson(product_id: Int,
                               store_id: Int,
                               is_dead: Boolean,
                               updated_on: String,
                               quantity: Int) {
  def removeNulls: InventoryAsLCBOJson = {
    // remove LCBO's poisoned null strings
    def notNull(s: String) = if (s eq null) "" else s

    InventoryAsLCBOJson(
      product_id,
      store_id,
      is_dead,
      notNull(updated_on),
      quantity)
  }
  // dbStoreProducts is assumed bound to a StoreId, so we can key simply by product_id
  def getProduct(dbStoreProducts: Map[Int, StoreProduct]) = dbStoreProducts.get(product_id)
}

class StoreProduct private() extends Record[StoreProduct] with KeyedRecord[Long] with CreatedUpdated[StoreProduct] {
  def meta = StoreProduct

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  lazy val product = MainSchema.productToStoreProducts.right(this)

  val storeid = new IntField(this)
  val productid = new IntField(this)
  val quantity = new IntField(this)

  def isDirty(inv: InventoryAsLCBOJson): Boolean =
    quantity.get != inv.quantity

  def copyAttributes(inv: InventoryAsLCBOJson): StoreProduct = {
    quantity.set(inv.quantity)
    this
  }
}

object StoreProduct extends StoreProduct with MetaRecord[StoreProduct] with pagerRestClient with Loggable {
  private val DBBatchSize = Props.getInt("storeProduct.DBBatchSize", 1)
  private implicit val formats = net.liftweb.json.DefaultFormats

  // thread-safe lock free objects
  private val storeProductsCache: concurrent.Map[(Int, Int), StoreProduct] = TrieMap()

  def update(storeId: Int, storeMap: Map[Int, StoreProduct]) =
    storeProductsCache ++= { for ( (prodId, sp) <- storeMap) yield (storeId, prodId) -> sp }

  // thread somewhat unsafe (harmless race condition because we never remove (so far) and if we miss an insert, it is just a normal timing issue, i.e. returning None just as another thread inserts)
  def getStoreProduct(storeId: Int, prodId: Int): Option[StoreProduct] = {
    val pair = (storeId, prodId)
    if (storeProductsCache.contains(pair)) {
      Some(storeProductsCache( pair))
    } else {
      None
    }
  }

  def create(inv: InventoryAsLCBOJson): StoreProduct =
    createRecord.
      storeid(inv.store_id).
      productid(inv.product_id).
      quantity(inv.quantity)

  @tailrec
  def updateStoreProducts(myStoreProducts: Iterable[StoreProduct]): Unit = {
    val slice = myStoreProducts.take(DBBatchSize)
    storeProducts.update(slice)
    val rest = myStoreProducts.takeRight(myStoreProducts.size - slice.size)
    if (!rest.isEmpty) updateStoreProducts( rest)
  }

  @tailrec
  def insertStoreProducts(myStoreProducts: Iterable[StoreProduct]): Unit = {
    val slice: Iterable[StoreProduct] = myStoreProducts.take(DBBatchSize)
    storeProducts.insert(slice)
    val rest = myStoreProducts.takeRight(myStoreProducts.size - slice.size)
    if (!rest.isEmpty) insertStoreProducts(rest)
  }

  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  def inventoryForStoreProduct(storeId: Int, productId: Int): InventoryAsLCBOJson = {
    val uri = s"$LcboDomainURL/stores/$storeId/products/$productId/inventory"
    logger.trace(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    (parse(pageContent) \ "result").extract[InventoryAsLCBOJson] // more throws
  }

}




