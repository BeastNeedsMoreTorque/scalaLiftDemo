package code.model

import code.model.MainSchema._
import net.liftweb.record.field.{LongField,IntField}
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.util.Props
import org.squeryl.annotations.Column

import scala.annotation.tailrec
import scala.collection.{Map, Iterable}

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

object StoreProduct extends StoreProduct with MetaRecord[StoreProduct] {
  private val DBBatchSize = Props.getInt("storeProduct.DBBatchSize", 1)

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
}




