package code.comet

import net.liftweb._
import http._
import actor._

/**
  * Created by philippederome on 2015-11-29.
 */
object ConfirmationExchange extends LiftActor with ListenerManager {
  private var msg: String = ""

  /**
      */
  def createUpdate = msg

  /**
    * We cache message minimally (to msg), and then update all the listeners.
   */
  override def lowPriority = {
    case s: String =>
      msg = s
      updateListeners()
  }
}


