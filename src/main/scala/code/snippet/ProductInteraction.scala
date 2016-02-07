package code.snippet

import code.model._
import code.snippet.SessionCache._
import net.liftweb.common._
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, SHtml}
import net.liftweb.json
import net.liftweb.json.JsonParser._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import scala.xml._

/**
  * This snippet contains state only via HTTP session (A Liftweb snippet is a web page fragment that has autonomous existence
  * equivalent to a page with framework
  * responsible to render all snippets on a page and to handle events between snippets and corresponding html div elements)
  * Much html and Javascript is generated here thanks to the capabilities of liftweb.
  * Created by philippederome on 15-10-26.
  */
object ProductInteraction extends Loggable {

  private val interactionsToImgMap: Map[String, String] = {
    val interactionsToImgMapAsStr = Props.get("product.interactionsImageMap", "") // get it in JSON format
    val jval: json.JValue = parse(interactionsToImgMapAsStr) // contains list of JField(String, JString(String))
    val v = for (elem <- jval.children.toVector) yield elem.values // contains vector of (String, String)
    v.map(_.asInstanceOf[(String, String)]).toMap  // could throw if contents that are configured are in wrong format (violating assumption of pairs (String,String)...
  }

  private val radioOptions: Seq[RadioElements] = RadioElements.radioOptions(  List("recommend", "consume", "cancel"), "cancel", interactionsToImgMap)

  private val hideProdDisplayJS =  JsHideId("prodDisplay")
  def setBorderJS(elt: String) = Call("toggleImage.frameRadioImage", "prodInteractionContainer", {elt})

  def render = {
    def transactionConfirmationJS = SetHtml("transactionConfirmation", Text(transactionConfirmation.is))
    /**
      * Generates a list of <tr></tr> elements as nodes of element prodAttributes within an assumed table
      *
      * @param p Product
      * @return a JsCmd that is JavaScript Lift will execute
      */
    def prodAttributesJS(p: Product, quantity: Int) = {
      val nodeSeq = for (x <-  p.createProductElemVals ++ List(("Quantity: ", quantity )) ) yield <tr><td class="prodAttrHead">{x._1}</td><td class="prodAttrContent">{x._2}</td></tr>
      SetHtml("prodAttributes", nodeSeq)
    }
    def prodDisplayJS(prod: Product, quantity: Int) =
      SetHtml("prodImgSpan", <img src={prod.imageThumbUrl.get}/>) &
      prodAttributesJS(prod, quantity) &
      JsShowId("prodDisplay")
    // Following 3 values are JavaScript objects to be executed when returning from Ajax call cb to browser to execute on browser
    // for the 3 events corresponding to the 3 buttons (for normal cases when there are no errors). We need to execute strictly Scala callbacks
    // here before invoking these JS callbacks. lazy val or def is required here because the value of Session variables changes as we handle events.
    def cancelCbJS =  transactionConfirmationJS & hideProdDisplayJS
    def consumeCbJS = hideProdDisplayJS & transactionConfirmationJS // meant to simulate consumption of product, or possibly a commentary on one

    def recommend() = {
      def maySelect(): JsCmd =
        if (theStoreId.is > 0 ) {
          // validates expected numeric input TheStore (a http session attribute) and when valid,
          // do real handling of accessing LCBO data
          transactionConfirmation.set("")
          val prodInv = Store.recommend(theStoreId.is, theCategory.is) match {
            // we want to distinguish error messages to user to provide better diagnostics.
            case Full(pair) => Full((pair._1, pair._2)) // returns prod and quantity in inventory normally
            case Failure(m, ex, _) => S.error(s"Unable to choose product of category ${theCategory.is} with message $m and exception error $ex"); Empty
            case Empty => S.error(s"Unable to choose product of category ${theCategory.is}"); Empty
          }
          prodInv.dmap { Noop }
          { pair: (Product, Int) => theProduct.set(Full(pair._1))
                          theProductInventory.set(pair._2)
                          S.error("") // work around clearCurrentNotices clear error message to make way for normal layout representing normal condition.
                          prodDisplayJS(pair._1, pair._2)
          }
        }
        else {
          S.error(s"We need to establish your local store first, please wait for local geo to be available or enter a postal code")
          Noop     // Error goes to site menu, but we could also send it to a DOM element if we were to specify an additional parameter
        }

      User.currentUser.dmap { S.error("recommend", "recommend feature unavailable, Login first!"); Noop }
      { currentUser =>
        theProduct.is.dmap { maySelect() & transactionConfirmationJS } // normal processing
        { p => S.notice("recommend", "Cancel or consume prior to a secondary recommendation")
          prodDisplayJS(p, theProductInventory.is) // ignore consecutive clicks for flow control, ensuring we take only the user's first click as actionable
          // for a series of clicks on button before we have time to disable it
        }
      }
    }

    def consume() = {
      def mayConsume(p: Product): JsCmd = {
        UserProduct.consume(p) match {
          case Full((userName, count)) =>
            transactionConfirmation.set(s"${p.name} has now been purchased $count time(s), $userName")
            theProduct.set(Empty)
            S.error("") // workaround clearCurrentNotices clears error message now that this is good, to get a clean screen.
          case Failure(x, ex, _) =>
            S.error(s"Unable to sell you product ${p.name} with error $x and exception '$ex'")
          case Empty =>
            S.error(s"Unable to sell you product ${p.name}")
        }
        Noop
      }

      // ignore consecutive clicks (when theProduct is defined), ensuring we do not attempt to consume multiple times in a row on a string of clicks from user
      theProduct.is.dmap { S.notice("consume", "Get a product recommendation before attempting to consume"); Noop}
      { p => mayConsume(p) & consumeCbJS } // Normal case: we got notified that we have a product that can be consumed and user expressed interest in consuming.
    }

    def cancel() = {
      transactionConfirmation.set("")
      theProduct.set(Empty)
      S.error("")
      cancelCbJS
    }

    ".options" #> LabelStyle.toForm( SHtml.ajaxRadio( radioOptions, Empty,
       (choice: RadioElements) => {
         choice.name match {
           case "consume" => consume() & setBorderJS(choice.name)
           case "recommend" => recommend() & setBorderJS(choice.name)
           case "cancel" => cancel() & setBorderJS(choice.name)
           case _ => Noop // for safety
        }
      }))
  }
}
