package code.model

import java.io.IOException
import java.net.SocketTimeoutException
import java.sql.SQLException
import scala.collection.IndexedSeq
import net.liftweb.common.Loggable

import net.liftweb.json._
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props
import org.apache.http.TruncatedChunkException
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2

/**
  * Created by philippederome on 2016-03-26.
  * storeid and productid are our composite PK whereas store_id and product_id is the same from LCBO's point of view with their PK.
  * We keep it in for referencing. See also case class InventoryAsLCBOJson further down.
  */
class Inventory private(val storeid: Long, val productid: Long, var quantity: Long, var updated_on: String, var is_dead: Boolean, var store_id: Int=0, var product_id: Int=0)
  extends KeyedEntity[CompositeKey2[Long,Long]] {

  def id = compositeKey(storeid, productid)

  def isDirty(inv: Inventory): Boolean =
    quantity != inv.quantity ||
    is_dead != inv.is_dead ||
    updated_on != inv.updated_on

  def copyDiffs(inv: Inventory): Inventory = {
    quantity = inv.quantity
    is_dead = inv.is_dead
    updated_on = inv.updated_on
    this
  }
}

object Inventory extends LCBOPageFetcher[Inventory] with ItemStateGrouper with Loggable {
  override def MaxPerPage = Props.getInt("inventory.lcboMaxPerPage", 0)
  private val dirtyPredicate: (Inventory, Inventory) => Boolean = {(x, y)=> x.isDirty(y)}

  case class InventoryAsLCBOJson(product_id: Int,
                                 store_id: Int,
                                 is_dead: Boolean,
                                 updated_on: String,
                                 quantity: Long) {}

  def apply( sKey: Long, pKey: Long, inv: InventoryAsLCBOJson) = {
    def notNull(s: String) = if (s eq null) "" else s  // protection against NullPointerException and LCBO's poisoning us with missing data
    new Inventory(
      storeid = sKey, // apply our composite PK
      productid = pKey, // apply our composite PK
      product_id = inv.product_id, // record their composite PK
      store_id = inv.store_id, // record their composite PK
      is_dead = inv.is_dead, // normal attributes from here on
      updated_on = notNull(inv.updated_on),
      quantity = inv.quantity
    )
  }

  final def extractItems(uri: String): (IndexedSeq[Inventory], JValue) = {
    val pageContent = get(uri)
    val jsonRoot = parse(pageContent)
    val nodes = (jsonRoot \ "result").children.toIndexedSeq // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = for (p <- nodes;
         inv = p.extract[InventoryAsLCBOJson] if !inv.is_dead;
         sKey <- Store.lcboIdToDBId(inv.store_id);
         pKey <- Product.lcboIdToDBId(inv.product_id);
         newInv = Inventory.apply(sKey, pKey, inv)
    ) yield newInv
    (items, jsonRoot)
  }

  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[MappingException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @throws(classOf[TruncatedChunkException])  // that's a brutal one. Caller must be ready to process these thrown exceptions.
  def fetchInventoriesByStore(uri: String, LcboStoreId: Long,
                              getCachedItem: (Inventory) => Option[Inventory],
                              mapByProductId: Map[Long, Inventory]): Unit = {

    // side effect to MainSchema.inventories cache (managed by Squeryl ORM)
    val items = collectItemsOnPages(uri, Seq("store_id" -> LcboStoreId))
    val (dirtyItems, newItems) = itemsByState[Inventory, Inventory](items, getCachedItem, dirtyPredicate)
    // identify the dirty ones for update and new ones for insert, clean up duplicate keys, store to DB and catch errors
    // update in memory COPIES of our inventories that have proven stale quantity to reflect the trusted LCBO up to date source
    val updatedInventories =
    { for (freshInv <- dirtyItems;
         cachedInv <- mapByProductId.get(freshInv.productid);
         dirtyInv = cachedInv.copyDiffs(freshInv) ) yield dirtyInv } // synch it up with copyDiffs

    // God forbid, we might supply ourselves data that violates composite key. Weed it out by taking one per composite key!
    def removeCompositeKeyDupes(invs: IndexedSeq[Inventory]) =
      invs.groupBy(inv => (inv.productid, inv.storeid)).map { case (k, v) => v.last }

    // filter the inventories that go to DB to remove dupes and keep a handle of them to help diagnose exceptions should we encounter them.
    val CompKeyFilterNewInventories = removeCompositeKeyDupes(newItems)
    val CompKeyFilterUpdatedInventories = removeCompositeKeyDupes(updatedInventories)

    try {
      // getNextException in catch is what is useful to log (along with the data that led to the exception)
      inTransaction {
        // we refresh just before splitting the inventories into a clean, dirty, new classes.
        MainSchema.inventories.update(CompKeyFilterUpdatedInventories)
        MainSchema.inventories.insert(CompKeyFilterNewInventories)
      }
    } catch {
      case se: SQLException =>
        // show me the data that caused the problem!
        logger.error(s"SQLException New Invs $CompKeyFilterNewInventories Dirty Invs $CompKeyFilterUpdatedInventories")
        logger.error("Code: " + se.getErrorCode)
        logger.error("SqlState: " + se.getSQLState)
        logger.error("Error Message: " + se.getMessage)
        logger.error("NextException:" + se.getNextException) // Show me why.
      // intentionally let propagate other errors, some of which could be fatal
    }
  }
}
