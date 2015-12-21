package code.snippet

import code.comet.{CSSUtils, ConfirmationExchange, ProductExchange}
import code.model._
import code.snippet.SessionCache.{TheCategory, TheStore}
import net.liftweb.common._
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.js.jquery.JqJsCmds.jsExpToJsCmd
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.util.{CssSel, Props}


/**
  * This snippet contains state only via HTTP session (A Liftweb snippet is a web page fragment that has autonomous existence equivalent to a page with framework
  * responsible to render them all and to handle events between snippets and corresponding html div elements) AND displayInstructions representing last data
  * received asynchronously as a CometListener, which updates this with context how it should render itself, namely enabling/disabling button.
  * Much html and Javascript is generated here thanks to the capabilities of liftweb. Similar general comments apply more or less for other comet listeners and to snippets.
  * Created by philippederome on 15-10-26.
  */
class ProductInteraction extends CSSUtils with Loggable {
  private val maxSampleSize = Props.getInt("product.maxSampleSize", 10)
  private var msg = ProductMessage() // private state indicating whether to show product when one is defined
  // and in this context here whether to enable or disable the Recommend button (empty means enable button, Full (product) being present means disable).
  private val toggleButtonsToConsumeJS = JsRaw("lcboViewer.toggleButtonPair( 'consume', 'recommend')")
  private val toggleButtonsToRecommendJS = JsRaw("lcboViewer.toggleButtonPair('recommend', 'consume')")
  private val addConsumeImgElem: CssSel = "#consume *+" #> <img src="/images/winesmall.png" alt="consume represented as glass of wine"/>
  private val addRecommendImgElem: CssSel = "#recommend *+" #> <img src="/images/recommend.png" alt="recommend represented as question mark"/>
  private val addCancelImgElem: CssSel = "#cancel *+" #> <img src="/images/cancel.png" alt="cancel represented as X"/>  // since above completely wiped out the XML (HTML5) node, we just lost the nice image with button, so add it back by adding a DOM element with img underneath the button.


  def render = {
    val buttonPairCssSel: CssSel = msg.product match {
      case Full(p) =>
        "#recommend" #> disable & // toggle to disable button. For Left case, we also toggle but don't need to explicitly enable since first instruction is to rewrite completely the DOM element and that removes the disabled attribute.
        "#consume" #> SHtml.ajaxButton("Consume", () =>  consume()) // rewrite the button DOM element by specifying its label and that its callback is AJAX and function consume, which will execute JS in browser when done
      case _ =>
        "#recommend" #> SHtml.ajaxButton( "Recommend", () =>  recommend()) &
        "#consume" #> disable  // disables button by setting required disabled attribute. We do this when there's nothing to display/consume
    }

    "* [onclick]" #> SHtml.ajaxCall(JsRaw("this.id"), { (s: String) =>
       s match {
         case "consume" => consume()
         case "recommend" => recommend()
         case "cancel" => cancel()
         case _ => Noop
       }
    }) & buttonPairCssSel & addCancelImgElem & addRecommendImgElem & addConsumeImgElem
 }

  def recommend(): JsCmd = {
    def maySelect(): JsCmd =
      TheStore.is.id match {
        // validates expected numeric input TheStore (a http session attribute) and when valid, do real handling of accessing LCBO data
        case s if s > 0 =>
          val prod = Product.recommend(maxSampleSize, s, TheCategory.is) match {
            // we want to distinguish error messages to user to provide better diagnostics.
            case util.Success(p) =>
              p or {
                S.notice(s"no product available for category ${TheCategory.is}")
                Empty
              } // returns prod normally but if empty, send a notice of error and return empty.
            case util.Failure(ex) => S.error(s"Unable to choose product of category ${TheCategory.is} with error $ex"); Empty
          }
          ConfirmationExchange ! "" // sends a clear string for the confirmation receiver comet actor in all cases since user clicked button.
          prod.dmap {
            Noop
          } { p: Product =>
            msg = ProductMessage(Full(p))
            ProductExchange ! msg // Sends out to Comet Actors AND SELF asynchronously the event that this product can now be rendered.
            S.clearCurrentNotices // clear error message to make way for normal layout representing normal condition.
          }
        case _ => S.error(s"Enter a number > 0 for Store") // Error goes to site menu, but we could also send it to a DOM element if we were to specify an additional parameter
      }
    msg.product match {
      case Full(p) => Noop // ignore consecutive clicks for flow control, ensuring we take only the user's first click as actionable for a series of clicks on button before we have time to disable it
      case _ => maySelect() // normal processing  (ProductConsume does it the other way around as it plays opposite role as to when it should be active)
        toggleButtonsToConsumeJS
    }
  }

  def consume(): JsCmd = {
    def mayConsume(p: Product): JsCmd = {
      Product.consume(p) match {
        case util.Success((userName, count)) =>
          ConfirmationExchange ! s"${p.name} has now been purchased $count time(s), $userName"
          msg = ProductMessage(Empty)
          ProductExchange ! msg // Sends out to other snippets or comet actors (and self to disable self button) aynchronously event to clear contents of a product display as it's no longer applicable
          S.clearCurrentNotices // clears error message now that this is good, to get a clean screen.
        case util.Failure(ex) => S.error(s"Unable to sell you product ${p.name} with error '$ex'")
      }
    }
    msg.product match {
      case Full(p)  => mayConsume(p) // we got notified that we have a product that can be consumed and user expressed interest in consuming. So, try it as a simulation by doing a DB store.
        jsExpToJsCmd(toggleButtonsToRecommendJS)
      case _ => Noop // ignore consecutive clicks, ensuring we do not attempt to consume multiple times in a row on a string of clicks from user
    }
  }

  def cancel(): JsCmd = {
    ConfirmationExchange ! ""
    msg = ProductMessage(Empty)
    ProductExchange ! msg // Sends out to other snippets asynchronously event to clear contents of a product display as it's no longer applicable
    jsExpToJsCmd(toggleButtonsToRecommendJS)
  }

}
