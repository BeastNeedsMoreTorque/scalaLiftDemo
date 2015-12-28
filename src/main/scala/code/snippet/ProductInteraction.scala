package code.snippet

import code.model._
import code.snippet.SessionCache._
import net.liftweb.common._
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, SHtml}
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
  private val maxSampleSize = Props.getInt("product.maxSampleSize", 10)

  private val interactionsToImgMap: Map[String, String] = {
    val interactionsToImgMapAsStr = Props.get("product.interactionsImageMap", "") // get it in JSON format
    val list = parse(interactionsToImgMapAsStr) // contains list of JField(String, JString(String))
    val v = for (elem <- list.children.toVector) yield elem.values // contains vector of (String, String)
    v.map(_.asInstanceOf[(String, String)]).toMap[String, String] // could throw if contents that are configured are in wrong format (violating assumption of pairs (String,String)...
  }

  private val radioOptions: Seq[RadioElements] = RadioElements.radioOptions(  List("recommend", "consume", "cancel"), "cancel", interactionsToImgMap)

  private val hideProdDisplayJS =  JsHideId("prodDisplay")
  def setBorderJS(elt: String) = Call("lcboViewer.frameRadioImage", "prodInteractionContainer", {elt})

  def render = {
    def transactionConfirmationJS = SetHtml("transactionConfirmation", Text(transactionConfirmation.is))
    def selectConfirmationJS(t: String) = SetHtml("selectConfirmation", Text(t))
    /**
      * Generates a list of <li></li> elements as nodes of element prodAttributes
      *
      * @param p Product
      * @return a JsCmd that is JavaScript Lift will execute
      */
    def prodAttributesJS(p: Product) = {
      val nodeSeq = for (x <- p.createProductLIElemVals) yield <li>{x}</li>
      SetHtml("prodAttributes", nodeSeq)
    }
    def prodDisplayJS(prod: Product) =
      SetHtml("prodImg", <img src={prod.imageThumbUrl}/>) &
      selectConfirmationJS(s"For social time, we suggest you: ${prod.name}") &
      prodAttributesJS(prod) &
      JsShowId("prodDisplay")
    // Following 3 values are JavaScript objects to be executed when returning from Ajax call cb to browser to execute on browser
    // for the 3 events corresponding to the 3 buttons (for normal cases when there are no errors). We need to execute strictly Scala callbacks
    // here before invoking these JS callbacks. lazy val or def is required here because the value of Session variables changes as we handle events.
    def cancelCbJS =  transactionConfirmationJS & hideProdDisplayJS
    def consumeCbJS =  selectConfirmationJS("") & hideProdDisplayJS & transactionConfirmationJS

    def recommend() = {
      def maySelect(): JsCmd =
        theStore.is.id match {
          // validates expected numeric input TheStore (a http session attribute) and when valid,
          // do real handling of accessing LCBO data
          case s if s > 0 =>
            transactionConfirmation.set("")
            val prod = Product.recommend(maxSampleSize, s, theCategory.is) match {
              // we want to distinguish error messages to user to provide better diagnostics.
              case Full(p) => Full(p) // returns prod normally but if empty, send a notice of error and return empty.
              case Failure(m, ex, ch) => S.error(s"Unable to choose product of category ${theCategory.is} with message $m and exception error $ex"); Empty
              case Empty => S.error(s"Unable to choose product of category ${theCategory.is}"); Empty
            }
            prod.dmap { Noop } { p: Product =>
                theProduct.set(Full(p))
                S.error("") // work around clearCurrentNotices clear error message to make way for normal layout representing normal condition.
                prodDisplayJS(p)
            }
          case _ => S.error(s"Enter a number > 0 for Store"); Noop
          // Error goes to site menu, but we could also send it to a DOM element if we were to specify an additional parameter
        }


      theProduct.is match {
        case Full(p) =>
          S.notice("recommend", "Cancel or consume prior to a secondary recommendation")
          prodDisplayJS(p)
        // ignore consecutive clicks for flow control, ensuring we take only the user's first click as actionable
        // for a series of clicks on button before we have time to disable it
        case _ => maySelect() & transactionConfirmationJS // normal processing
      }
    }

    def consume() = {
      def mayConsume(p: Product): JsCmd = {
        Product.consume(p) match {
          case Full((userName, count)) =>
            transactionConfirmation.set(s"${p.name} has now been purchased $count time(s), $userName")
            theProduct.set(Empty)
            S.error("") // workaround clearCurrentNotices clears error message now that this is good, to get a clean screen.
            Noop
          case Failure(x, ex, ch) =>
            S.error(s"Unable to sell you product ${p.name} with error $x and exception '$ex'")
            Noop
          case Empty =>
            S.error(s"Unable to sell you product ${p.name}")
            Noop
        }
      }

      theProduct.is match {
        case Full(p)  => mayConsume(p) & consumeCbJS
          // we got notified that we have a product that can be consumed and user expressed interest in consuming.
          // So, try it as a simulation by doing a DB store.
        case _ => S.notice("consume", "Get a product recommendation before attempting to consume"); Noop
        // ignore consecutive clicks, ensuring we do not attempt to consume multiple times in a row on a string of clicks from user
      }
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
