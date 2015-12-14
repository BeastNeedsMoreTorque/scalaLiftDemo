package code.comet

import net.liftweb.common.{Full, Loggable}
import net.liftweb.http.{CometListener, CometActor}
import net.liftweb.util.Helpers._
/**
  * Created by philippederome on 2015-12-01.
  */
class ConfirmationReceiver extends CometActor with CometListener with Loggable {
  private var confirm: String = "" // private state

  // register this component with the Actor in order to receive notifications
  def registerWith = ConfirmationExchange

  import scala.language.postfixOps
  override def lifespan = Full(120 seconds)

  // listen for data published by other snippets notifying us of new data to act upon.
  override def lowPriority = {
    // use partial function
    case m: String =>
      confirm = m
      reRender()
  }

  // render the component by showing confirmation (string) content
  def render = "#confirmMsg" #> confirm
}

