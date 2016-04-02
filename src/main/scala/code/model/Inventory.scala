package code.model

import java.io.IOException
import java.net.SocketTimeoutException
import java.sql.SQLException
import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer
import net.liftweb.common.Loggable

import net.liftweb.json._
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Props
import org.apache.http.TruncatedChunkException
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2

/**
  * Created by philippederome on 2016-03-26.
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

object Inventory extends LCBOPageFetcher[Inventory] with ItemStateGrouper[Inventory, Inventory] with Loggable {
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
      storeid = sKey,
      productid = pKey,
      product_id = inv.product_id,
      store_id = inv.store_id,
      is_dead = inv.is_dead,
      updated_on = notNull(inv.updated_on),
      quantity = inv.quantity
    )
  }

  final def extractItems(uri: String): (IndexedSeq[Inventory], JValue) = {
    val pageContent = get(uri)
    val jsonRoot = parse(pageContent)
    val nodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = ArrayBuffer[Inventory]()
    for (p <- nodes;
         inv = p.extract[InventoryAsLCBOJson] if !inv.is_dead;
         sKey <- Store.lcboIdToDBId(inv.store_id);
         pKey <- Product.lcboIdToDBId(inv.product_id)
    ) { items += Inventory.apply(sKey, pKey, inv) }
    (items, jsonRoot)
  }

  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[MappingException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @throws(classOf[TruncatedChunkException])  // that's a brutal one.
  def fetchInventoriesByStore(uri: String, storeLcboId: Long, storeKey: Long,
                              getCachedItem: (Inventory) => Option[Inventory],
                              mapByProductId: Map[Long, Inventory]): Unit = {

    inTransaction {
      // side effect to MainSchema.inventories cache (managed by Squeryl ORM)
      val items = collectItemsOnPages(uri, Seq("store_id" -> storeLcboId))
      // partition items into 4 lists, clean (no change), new (to insert) and dirty (to update) and undefined (invalid/unknown product, ref.Integ risk), using neat groupBy
      val inventoriesByState = itemsByState(items, getCachedItem, dirtyPredicate)

      // update in memory COPIES of our inventories that have proven stale quantity to reflect the trusted LCBO up to date source
      val dirtyInventories = ArrayBuffer[Inventory]()
      for (srcInvs <- inventoriesByState.get(EntityRecordState.Dirty);
           srcInv <- srcInvs;
           dbInv <- mapByProductId.get(srcInv.productid)) {
        dirtyInventories += dbInv.copyDiffs(srcInv)   // not a replacement, rather a copy!
      }
      // create new inventories we didn't know about
      val newInventories = inventoriesByState.get(EntityRecordState.New).map( identity).getOrElse(IndexedSeq())

      // God forbid, we might supply ourselves data that violates composite key. Weed it out!
      def removeCompositeKeyDupes(invs: IndexedSeq[Inventory]) =
        invs.groupBy(x => (x.productid, x.storeid)).map { case (k, v) => v.last }

      // filter the inventories that go to DB to remove dupes and keep a handle of them to help diagnose exceptions should we encounter them.
      val CompKeyFilterNewInventories = removeCompositeKeyDupes(newInventories)
      val CompKeyFilterDirtyInventories = removeCompositeKeyDupes(dirtyInventories)

      try {
        // getNextException in catch is what is useful to log (along with the data that led to the exception)
        inTransaction {
          // we refresh just before splitting the inventories into clean, dirty, new classes.
          MainSchema.inventories.update(CompKeyFilterDirtyInventories)
          MainSchema.inventories.insert(CompKeyFilterNewInventories)
        }
      } catch {
        case se: SQLException => // the bad
          // show me the data that caused the problem!
          logger.error(s"SQLException New Invs $CompKeyFilterNewInventories Dirty Invs $CompKeyFilterDirtyInventories")
          logger.error("Code: " + se.getErrorCode)
          logger.error("SqlState: " + se.getSQLState)
          logger.error("Error Message: " + se.getMessage)
          logger.error("NextException:" + se.getNextException) // the "good". Show me why.
        case e: Exception => // the UGLY!
          logger.error("General exception caught: " + e) // we'll pay the price on that one.
      }
    }
  }
}
