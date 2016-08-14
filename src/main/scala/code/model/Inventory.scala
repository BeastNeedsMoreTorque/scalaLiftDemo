package code.model

import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.pageFetcher.{LCBOPageFetcherComponentImpl, LCBOPageLoader}
import code.model.utils.KeyHolder
import code.model.utils.RetainSingles.implicitSeqToRetainSingles
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.common.Loggable
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2

import scala.collection.{IndexedSeq, Iterable}
import cats.syntax.xor._
import cats.data.Xor

object DefaultDateAsNow {
  def defaultDate: String = formattedDateNow.replace('/', '-') // Lift uses / but LCBO uses - so standardizes it for minimal changes.
}
class InventoryAsLCBOJson(var product_id: Long,
                          var store_id: Long,
                          var is_dead: Boolean,
                          var updated_on: Option[String], // Not always provided by LCBO
                          var quantity: Long)  {
  this.updated_on = Some(updated_on.fold(DefaultDateAsNow.defaultDate)(identity)) // provide default of today when not provided.
  def this() = this(0, 0, false, Some(DefaultDateAsNow.defaultDate), 0)
  def copy(inv: InventoryAsLCBOJson): Unit = {
    this.product_id = inv.product_id
    this.store_id = inv.store_id
    this.is_dead = inv.is_dead
    this.updated_on = inv.updated_on
    this.quantity = inv.quantity
  }
}

/**
  * Created by philippederome on 2016-03-26.
  * storeid and productid are our composite PK whereas store_id and product_id is the same from LCBO's point of view with their PK.
  * We keep it in for referencing. See also case class InventoryAsLCBOJson further down.
  */
case class Inventory private(val storeid: Long,
                             val productid: Long)
  extends InventoryAsLCBOJson with KeyedEntity[CompositeKey2[Long,Long]] with KeyHolder {

  override def getKey: String = s"$productid:$storeid"

  def id: CompositeKey2[Long, Long] = compositeKey(storeid, productid)

  def copyDiffs(inv: Inventory): Inventory = {
    quantity = inv.quantity
    is_dead = inv.is_dead
    updated_on = inv.updated_on
    this
  }

  override def toString: String = s"$storeid $productid $quantity $updated_on $is_dead $store_id $product_id"
}

case class UpdatedAndNewInventories(updatedInvs: Iterable[Inventory], newInvs: Iterable[Inventory])

object Inventory extends LCBOPageLoader with LCBOPageFetcherComponentImpl with ItemStateGrouper with ORMExecutor with Loggable {
  val MaxPerPage = Props.getInt("inventory.lcboMaxPerPage", 0)
  implicit val formats = net.liftweb.json.DefaultFormats
  val extract: JSitemsExtractor[Inventory] =  { jVal =>
    for {p <- jVal.children.toIndexedSeq
         inv <- p.extractOpt[InventoryAsLCBOJson]
         storeid <- Store.lcboIdToPKMap.get(LCBO_ID(inv.store_id))
         productid <- Product.lcboIdToPKMap.get(LCBO_ID(inv.product_id))
         newInv = Inventory(storeid, productid, inv)
    } yield newInv
  }

  def apply(storeid: Long, productid: Long, inv: InventoryAsLCBOJson): Inventory = {
    val obj = new Inventory(storeid, productid)
    obj.copy(inv)
    obj
  }

  def fetchInventoriesByStore(webApiRoute: String,
                              get: (Inventory) => Option[Inventory],
                              mapByProductId: Map[P_KEY, Inventory],
                              params: Seq[(String, Any)]): Throwable Xor UpdatedAndNewInventories =  {
    // set up some functional transformers first, then get ready for real work.
    // God forbid, we might supply ourselves data that violates composite key. Weed it out by taking one per composite key!

    def getUpdatedInvs(items: IndexedSeq[Inventory]) = {
      { for {freshInv <- items
             cachedInv <- mapByProductId.get(P_KEY(freshInv.productid))
             dirtyInv = cachedInv.copyDiffs(freshInv) } yield dirtyInv }
    }

    for {items <- collectItemsAsWebClient(webApiRoute, extract, params :+ "per_page" -> MaxPerPage)
      updatesAndInserts <- (itemsByState[Inventory, Inventory](items, get)).right[Throwable]
      updatedInventories <- (getUpdatedInvs(updatesAndInserts.updates).retainSingles).right[Throwable]
      newInventories <- updatesAndInserts.inserts.retainSingles.right[Throwable]
      inventories <- UpdatedAndNewInventories(updatedInventories, newInventories).right[Throwable]} yield inventories
  }
}
