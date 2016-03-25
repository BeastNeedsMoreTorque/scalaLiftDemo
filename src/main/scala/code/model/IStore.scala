package code.model

import scala.collection.Iterable
import net.liftweb.common.Box

/**
  * Created by philippederome on 2016-03-25.
  */
trait IStore {
  def recommend(category: String, requestSize: Int): Box[Iterable[(Long, IProduct)]]

}
