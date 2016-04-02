package code.model

import java.io.IOException
import java.net.SocketTimeoutException
import java.sql.SQLException

import net.liftweb.common.Loggable
import net.liftweb.json.MappingException
import net.liftweb.squerylrecord.RecordTypeMode._
import org.apache.http.TruncatedChunkException
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2

import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer

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
    quantity != inv.quantity ||
    is_dead != inv.is_dead ||
    updated_on != inv.updated_on

  def copyAttributes(inv: InventoryAsLCBOJson): Inventory = {
    def notNull(s: String) = if (s eq null) "" else s

    quantity = inv.quantity
    updated_on = notNull(inv.updated_on)
    is_dead = inv.is_dead
    this
  }
}

object Inventory extends Loggable {
  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[MappingException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @throws(classOf[TruncatedChunkException])  // that's a brutal one.
  def fetchInventoriesByStore(uri: String, storeLcboId: Long, storeKey: Long, mapByProductId: Map[Long, Inventory]) =
    inTransaction {
    // side effect to MainSchema.inventories cache (managed by Squeryl ORM)

    def stateOfInventory(item: InventoryAsLCBOJson): EnumerationValueType = {
      val pKey = Product.lcboIdToDBId(item.product_id)
      val invOption = for (x <- pKey;
                           inv <- mapByProductId.get(x)) yield inv
      (pKey, invOption) match {
        case (Some(id), None)  => EntityRecordState.New
        case (Some(id), Some(inv)) if inv.isDirty(item) => EntityRecordState.Dirty
        case (Some(id), _) => EntityRecordState.Clean
        case _ => EntityRecordState.Undefined  // on a product we don't yet know, so consider it as undefined so we don't violate FK on products (LCBO makes no guaranty to be consistent here)
      }
    }

    val items = InventoryFetcher.collectItemsOnPages( uri, Seq("store_id" -> storeLcboId))
    // partition items into 4 lists, clean (no change), new (to insert) and dirty (to update) and undefined (invalid/unknown product, ref.Integ risk), using neat groupBy
    val inventoriesByState = items.groupBy( stateOfInventory )

    // update in memory our inventories with stale quantity to reflect the trusted LCBO up to date source
    val dirtyInventories = ArrayBuffer[Inventory]()
    for (jsInvs <- inventoriesByState.get(EntityRecordState.Dirty);
         jsInv <- jsInvs;
         prodKey <- Product.lcboIdToDBId(jsInv.product_id);
         dbInv <- mapByProductId.get(prodKey))
    { dirtyInventories += dbInv.copyAttributes(jsInv) }

    // create new inventories we didn't know about
    val newInventories = ArrayBuffer[Inventory]()
    for (jsInvs <- inventoriesByState.get(EntityRecordState.New);
         jsInv <- jsInvs;
         prodKey <- Product.lcboIdToDBId(jsInv.product_id))
    { newInventories += new Inventory(storeKey, prodKey, jsInv.quantity, jsInv.updated_on, jsInv.is_dead ) }

    // God forbid, we might supply ourselves data that violates composite key. Weed it out!
    def removeCompositeKeyDupes(invs: IndexedSeq[Inventory]) =
      invs.groupBy(x => (x.productid, x.storeid)).map{ case (k,v) => v.last }

    // filter the inventories that go to DB to remove dupes and keep a handle of them to help diagnose exceptions should we encounter them.
    val CompKeyFilterNewInventories = removeCompositeKeyDupes(newInventories)
    val CompKeyFilterDirtyInventories = removeCompositeKeyDupes(dirtyInventories)

    try {  // getNextException in catch is what is useful to log (along with the data that led to the exception)
      inTransaction {
        // we refresh just before splitting the inventories into clean, dirty, new classes.
        MainSchema.inventories.update(CompKeyFilterDirtyInventories)
        MainSchema.inventories.insert(CompKeyFilterNewInventories)
      }
    } catch {
      case se: SQLException =>  // the bad
        // show me the data that caused the problem!
        logger.error(s"SQLException New Invs $CompKeyFilterNewInventories Dirty Invs $CompKeyFilterDirtyInventories")
        logger.error("Code: " + se.getErrorCode)
        logger.error("SqlState: " + se.getSQLState)
        logger.error("Error Message: " + se.getMessage)
        logger.error("NextException:" + se.getNextException)  // the "good". Show me why.
      case e: Exception =>  // the UGLY!
        logger.error("General exception caught: " + e) // we'll pay the price on that one.
    }
  }
}
