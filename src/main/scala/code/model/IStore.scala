package code.model

import scala.collection.Iterable
import net.liftweb.common.Box
import code.model.GlobalLCBO_IDs.P_KEY

/**
  * Created by philippederome on 2016-03-25.
  */
trait IStore extends Equals with KeyKeeper {
  def Name: String
  def isDead: Boolean
  def addressLine1: String

  def recommend(category: String,
                requestSize: Int,
                fetcher: ProductFetcher): Box[Iterable[(IProduct, Long)]]

  // @see Scala in Depth
  override def canEqual(other: Any) =
    other.isInstanceOf[IStore]

  override def hashCode: Int = Name.## // if the names are the same, they're probably the same

  override def equals(other: Any): Boolean = {
    other match {
      case that: IStore =>
        if (this eq that) true
        else {
          that.## == this.## &&
          that.canEqual(this) &&
          ( Name == that.Name &&
            isDead == that.isDead &&
            addressLine1 == that.addressLine1)
        }
      case _ => false
    }
  }

  val dirtyPredicate: (IStore, IStore) => Boolean = {(x, y)=> !x.equals(y)}
  def getCachedItem: IStore => Option[IStore]
  val inventoryByProductIdMap: P_KEY => Option[Inventory]
  def getProductsByCategory(lcboCategory: String): IndexedSeq[IProduct]
  def asyncLoadCache(): Unit

}
