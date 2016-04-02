package code.model

import net.liftweb.squerylrecord.RecordTypeMode._
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2

/**
  * Created by philippederome on 2016-03-26.
  */
case class InventoryAsLCBOJson(product_id: Int,
                               store_id: Int,
                               is_dead: Boolean,
                               updated_on: String,
                               quantity: Int) {}

class Inventory(val storeid: Long, val productid: Long, var quantity: Long, var updated_on: String, var is_dead: Boolean)
  extends KeyedEntity[CompositeKey2[Long,Long]] {

  def id = compositeKey(storeid, productid)

  def isDirty(inv: InventoryAsLCBOJson): Boolean =
    quantity != inv.quantity

  def notNull(s: String) = if (s eq null) "" else s

  def copyAttributes(inv: InventoryAsLCBOJson): Inventory = {
    quantity = inv.quantity
    updated_on = notNull(inv.updated_on)
    is_dead = inv.is_dead
    this
  }
}
