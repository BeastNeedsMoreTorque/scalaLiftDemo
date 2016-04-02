package code.model

import scala.collection.Iterable
import net.liftweb.common.Box

/**
  * Created by philippederome on 2016-03-25.
  */
trait IStore  {
  def recommend(category: String, requestSize: Int): Box[Iterable[(Long, IProduct)]]
  def isDead: Boolean
  def addressLine1: String

  override def equals(o: Any) = o match {
    case that: IStore => isDead == that.isDead &&
      addressLine1 == that.addressLine1
    case _ => false
  }

}
