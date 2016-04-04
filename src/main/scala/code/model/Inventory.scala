package code.model

import java.io.IOException
import java.net.SocketTimeoutException

import scala.collection.{IndexedSeq, Iterable}
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

object Inventory extends LCBOPageFetcher[Inventory] with ItemStateGrouper with ORMBatchExecutor with Loggable {
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
         inv = p.extract[InventoryAsLCBOJson];
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
  def fetchInventoriesByStore(uri: String,
                              getCachedItem: (Inventory) => Option[Inventory],
                              mapByProductId: Map[Long, Inventory],
                              params: Seq[(String, Any)]): Unit = {
    // side effect to MainSchema.inventories cache (managed by Squeryl ORM)

    // set up some functional transformers first, then get ready for real work.
    // God forbid, we might supply ourselves data that violates composite key. Weed it out by taking one per composite key!
    def removeCompositeKeyDupes(invs: IndexedSeq[Inventory]) =
      invs.groupBy(inv => (inv.productid, inv.storeid)).map { case (_, idxSeq) => idxSeq(0) }
    def getUpdatedInvs(items: IndexedSeq[Inventory]) = {
      { for (freshInv <- items;
             cachedInv <- mapByProductId.get(freshInv.productid);
             dirtyInv = cachedInv.copyDiffs(freshInv) ) yield dirtyInv }
    }
    def inventoryTableUpdater: (Iterable[Inventory]) => Unit = MainSchema.inventories.update _
    def inventoryTableInserter: (Iterable[Inventory]) => Unit = MainSchema.inventories.insert _
    val getDBReadyUpdatedInvs: (IndexedSeq[Inventory] => Iterable[Inventory]) = removeCompositeKeyDupes _ compose getUpdatedInvs _  // more of a toy here than anything; interesting to know we can compose.

    val items = collectItemsOnPages(uri, params)
    val (dirtyItems, newItems) = itemsByState[Inventory, Inventory](items, getCachedItem, dirtyPredicate)
    val updatedInventories = getDBReadyUpdatedInvs(dirtyItems)
    val newInventories = removeCompositeKeyDupes(newItems)
    inTransaction {
      execute[Inventory](updatedInventories, inventoryTableUpdater)  // bulk update the ones needing an update, having made the change from LCBO input
      execute[Inventory](newInventories, inventoryTableInserter) // bulk insert the ones needing an insert having filtered out duped composite keys
    }
  }
}
