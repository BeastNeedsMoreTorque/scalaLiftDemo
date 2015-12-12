package code.comet

import code.model.{ClearInstruction, Product}
import net.liftweb._
import http._
import actor._

/**
  * Created by philippederome on 2015-11-29.
 * A singleton that provides Product exchange features to all clients.
 * It's an Actor so it's thread-safe because only one
 * "product" item will be processed at once ("product" because it might represent nothing as Left part of Either as ClearInstruction).
 */
object ProductExchange extends LiftActor with ListenerManager {
  private var data: Either[ClearInstruction, Product] = Left(ClearInstruction())
  // private state
  private var currentIsProd = false

  /**
    * When we update the listeners, what do we send?
   * We send a Either, which is immutable type,
   * so it can be shared with lots of threads without any
   * danger or locking. One strong argument against mutable types, which I don't use directly in this project.
   */
  def createUpdate = data

  /**
   * process product/ClearInstruction that are sent to the Actor.  In
   * this case, we're looking for a Either that is sent
   * to the ProductExchange.  We cache it minimally, and then update all the listeners.
    */
  override def lowPriority = {
      // use partial function for the callback to our publisher ProductExchange, we filter one type of data, cache it so that upon rendering we capture it and act accordingly
      case p: Either[ClearInstruction, Product] => data = p; updateListeners()
    }
}