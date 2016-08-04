package code.model

import cats.data.Xor

import scala.collection.Iterable
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.utils.RNG

trait InventoryService extends KeyKeeper {
  val inventoryByProductIdMap: P_KEY => Option[Inventory]
  def getProduct(x: LCBO_ID): Option[IProduct]
  def getProductKeysByCategory(lcboCategory: String): IndexedSeq[KeyKeeperVals]
  def asyncLoadCache(): Unit
}
/**
  * Created by philippederome on 2016-03-25.
  */
trait IStore extends InventoryService {
  def Name: String
  def isDead: Boolean
  def addressLine1: String

  def advise(rng: RNG, category: String, requestSize: Int, runner: ProductRunner): Xor[Throwable, Iterable[(IProduct, Long)]]
}
