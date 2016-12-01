package code.model

import code.model.GlobalLCBO_IDs._
import code.model.pageFetcher.{LCBOPageFetcherComponentImpl, LCBOPageLoader}
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.common.Loggable
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import org.squeryl.KeyedEntity
import org.squeryl.dsl.CompositeKey2

import scala.collection.Iterable
import code.model.utils.RetainSingles._
import cats.implicits._
import code.model.utils.KeyHolder

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
  extends InventoryAsLCBOJson with KeyedEntity[CompositeKey2[Long,Long]] {

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
         storeid <- Store.lcboKeyToPKMap.get(inv.store_id.LcboKeyID)
         productid <- Product.lcboKeyToPKMap.get(inv.product_id.LcboKeyID)
         newInv = Inventory(storeid, productid, inv)
    } yield newInv
  }

  def apply(storeid: Long, productid: Long, inv: InventoryAsLCBOJson): Inventory = {
    val obj = new Inventory(storeid, productid)
    obj.copy(inv)
    obj
  }

  def loadInventoriesByStore(webApiRoute: String,
                              get: (Inventory) => Option[Inventory],
                              mapByProductId: Map[P_KEY, Inventory],
                              params: Seq[(String, Any)]): Either[Throwable, UpdatedAndNewInventories] =  {
    // set up some functional transformers first, then get ready for real work.
    // God forbid, we might supply ourselves data that violates composite key. Weed it out by taking one per composite key!

    def getUpdatedInvs(items: Iterable[Inventory]) = for {
      freshInv <- items
      cachedInv <- mapByProductId.get(freshInv.productid.PKeyID)
      dirtyInv = cachedInv.copyDiffs(freshInv)
    } yield dirtyInv

    implicit val keyHolder: KeyHolder[Inventory] = KeyHolder.getKey { inv => s"${inv.productid}:${inv.storeid}" }
    for {items <- collectItemsAsWebClient(webApiRoute, extract, params :+ "per_page" -> MaxPerPage)
      updatesAndInserts <- Right(itemsByState[Inventory, Inventory](items, get))
      updatedInventories <- Right(getUpdatedInvs(retainSingles(updatesAndInserts.updates)))
      newInventories <- Right(retainSingles(updatesAndInserts.inserts))
      inventories <- Right(UpdatedAndNewInventories(updatedInventories, newInventories))
    } yield inventories
  }
}
