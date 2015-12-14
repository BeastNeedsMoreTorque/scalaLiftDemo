package code.comet

import net.liftweb._
import http._
import net.liftweb.common.{Empty, Full, Loggable}
import code.model.ProductMessage
import net.liftweb.util.ClearClearable
import net.liftweb.util.Helpers._

/**
  * The comet ProductDisplay component
  */
class ProductDisplay extends CometActor with CometListener with Loggable {
  private var msg = ProductMessage(Empty) // private state

  // register this component with the Actor in order to receive notifications
  def registerWith = StoreProductExchange

  import scala.language.postfixOps
  override def lifespan = Full(120 seconds)

  // listen for data published by other snippets notifying us of new data to act upon.
  override def lowPriority = {
    // use partial function
    case p: ProductMessage => msg = p; reRender()
  }

  // render the component by showing product content if that's the latest available or nothing if we received a clear instruction (Empty Product Box)
  def render = {
    msg.product match {
      case Full(p) =>
        "#selectConfirmation" #> s"For social time, we suggest you: ${p.name}" & // assigns RHS text message as value of DOM element selectConfirmation
          "#productImg [src]" #> p.imageThumbUrl & // add productImg DOM element will have html attribute src set to the RHS image thumb URL.
          "li *" #> p.createProductLIElemVals & // generates many html li items on the fly one per list entry.
          ClearClearable // ensures list items appear only once (not duplicated)
      case _ =>  "*" #> "" // intentionally wipes out the data to nothing
    }
  }
}
