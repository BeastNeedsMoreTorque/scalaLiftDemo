package code.model

import net.liftweb.record.field.LongField
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord

import org.squeryl.annotations.Column

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
}

case class ProductWithInventory(productId: Int, quantity: Int)

class Inventory private() extends Record[Inventory] with KeyedRecord[Long] with CreatedUpdated[Inventory] {
  def meta = Inventory

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  lazy val product = MainSchema.productToStoreProducts.rightStateful(this)
  lazy val store = MainSchema.storeToInventories.rightStateful(this)

  val storeid = new LongField(this)
  val productid = new LongField(this)
  val quantity = new LongField(this)

  def isDirty(inv: InventoryAsLCBOJson): Boolean =
    quantity.get != inv.quantity

  def copyAttributes(inv: InventoryAsLCBOJson): Inventory = {
    quantity.set(inv.quantity)
    this
  }
}

object Inventory extends Inventory with MetaRecord[Inventory] {
  def create(qty: Long, sid: Long, pid: Long): Inventory =
    createRecord.
      quantity(qty).
      storeid(sid).
      productid(pid)

}
