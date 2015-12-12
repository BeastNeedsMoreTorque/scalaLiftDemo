package code.comet

import code.model.{DisplayStoreInstruction, Store}
import net.liftweb.actor.LiftActor
import net.liftweb.http.ListenerManager

/**
  * Created by philippederome on 2015-12-09.
  */
object StoreExchange extends LiftActor with ListenerManager {
  def createUpdate = DisplayStoreInstruction()

  override def lowPriority = {
    case i: DisplayStoreInstruction =>
      updateListeners()
  }
}