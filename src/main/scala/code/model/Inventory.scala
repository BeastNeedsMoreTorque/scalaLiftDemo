package code.model

import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.pageFetcher.{LCBOPageFetcherComponentImpl, LCBOPageLoader}
import code.model.utils.KeyHolder
import code.model.utils.RetainSingles.retainSinglesImpure
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.common.Loggable
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2

import scala.collection.{IndexedSeq, Iterable}
import scala.util.Try


/**
  * Created by philippederome on 2016-03-26.
  * storeid and productid are our composite PK whereas store_id and product_id is the same from LCBO's point of view with their PK.
  * We keep it in for referencing. See also case class InventoryAsLCBOJson further down.
  */
class Inventory private(val storeid: Long,
                        val productid: Long,
                        var quantity: Long,
                        var updated_on: String,
                        var is_dead: Boolean,
                        var store_id: Long=0,
                        var product_id: Long=0)
  extends Equals with KeyedEntity[CompositeKey2[Long,Long]] with KeyHolder {

  override def getKey: String = s"$productid:$storeid"

  def id: CompositeKey2[Long, Long] = compositeKey(storeid, productid)

  override def equals(other: Any): Boolean =
    other match {
      case that: Inventory =>
        (this eq that) ||
          (that.canEqual(this) &&
            quantity == that.quantity &&
            is_dead == that.is_dead &&
            updated_on == that.updated_on)
      case _ => false
    }

  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[Inventory]

  def copyDiffs(inv: Inventory): Inventory = {
    quantity = inv.quantity
    is_dead = inv.is_dead
    updated_on = inv.updated_on
    this
  }

  override def toString: String = s"$storeid $productid $quantity $updated_on $is_dead $store_id $product_id"
}

case class UpdatedAndNewInventories(updatedInvs: Iterable[Inventory], newInvs: Iterable[Inventory]) {}

object Inventory extends LCBOPageLoader with LCBOPageFetcherComponentImpl with ItemStateGrouper with ORMExecutor with Loggable {
  val MaxPerPage = Props.getInt("inventory.lcboMaxPerPage", 0)
  implicit val formats = net.liftweb.json.DefaultFormats
  val extract: JSitemsExtractor[Inventory] =  { jVal =>
    for (p <- jVal.children.toIndexedSeq;
         inv <- p.extractOpt[InventoryAsLCBOJson];
         sKey <- Store.lcboIdToPKMap.get(LCBO_ID(inv.store_id));
         pKey <- Product.lcboIdToPKMap.get(LCBO_ID(inv.product_id));
         newInv = Inventory.apply(sKey, pKey, inv)
    ) yield newInv
  }

  def apply( sKey: Long, pKey: Long, inv: InventoryAsLCBOJson): Inventory = {
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

  def fetchInventoriesByStore(webApiRoute: String,
                              get: (Inventory) => Option[Inventory],
                              mapByProductId: Map[P_KEY, Inventory],
                              params: Seq[(String, Any)]): Try[UpdatedAndNewInventories] =  {
    // set up some functional transformers first, then get ready for real work.
    // God forbid, we might supply ourselves data that violates composite key. Weed it out by taking one per composite key!

    def getUpdatedInvs(items: IndexedSeq[Inventory]) = {
      { for (freshInv <- items;
             cachedInv <- mapByProductId.get(P_KEY(freshInv.productid));
             dirtyInv = cachedInv.copyDiffs(freshInv) ) yield dirtyInv }
    }

    for (items <- Try(collectItemsAsWebClient(webApiRoute, extract, params :+ "per_page" -> MaxPerPage));
         dirtyAndNewItems <- Try(itemsByState[Inventory, Inventory](items, get));
         updatedInventories <- Try(retainSinglesImpure(getUpdatedInvs(dirtyAndNewItems.dirtys)));
         newInventories <- Try(retainSinglesImpure(dirtyAndNewItems.news));
         inventories <- Try(UpdatedAndNewInventories(updatedInventories, newInventories))) yield inventories
  }

  case class InventoryAsLCBOJson(product_id: Long,
                                 store_id: Long,
                                 is_dead: Boolean,
                                 updated_on: String,
                                 quantity: Long) {}
}
