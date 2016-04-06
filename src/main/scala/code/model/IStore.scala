package code.model

import code.model.GlobalLCBO_IDs.LCBO_ID

import scala.collection.Iterable
import net.liftweb.common.Box

/**
  * Created by philippederome on 2016-03-25.
  */
trait IStore  {
  def lcboId: LCBO_ID

  def recommend(category: String, requestSize: Int): Box[Iterable[(IProduct, Long)]]
  def isDead: Boolean
  def addressLine1: String

  override def equals(o: Any) = o match {
    case that: IStore => isDead == that.isDead &&
      addressLine1 == that.addressLine1
    case _ => false
  }
  val dirtyPredicate: (IStore, IStore) => Boolean = {(x, y)=> !x.equals(y)}
  def getCachedItem: (IStore) => Option[IStore]

}
