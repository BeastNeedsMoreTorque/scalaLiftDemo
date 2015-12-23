package code.snippet

import code.model._
import code.snippet.SessionCache._
import net.liftweb.common._
import net.liftweb.http.js.JE._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, SHtml}
import net.liftweb.util.Helpers._
import net.liftweb.util.{ClearClearable, CssSel, Props}
import net.liftweb.common.Box
import scala.xml.Text

/**
  * This snippet contains state only via HTTP session (A Liftweb snippet is a web page fragment that has autonomous existence
  * equivalent to a page with framework
  * responsible to render them all and to handle events between snippets and corresponding html div elements)
  * AND displayInstructions representing last data
  * received asynchronously as a CometListener, which updates this with context how it should render itself,
  * namely enabling/disabling button.
  * Much html and Javascript is generated here thanks to the capabilities of liftweb.
  * Similar general comments apply more or less for other comet listeners and to snippets.
  * Created by philippederome on 15-10-26.
  */
class ProductInteraction extends Loggable {
  private val maxSampleSize = Props.getInt("product.maxSampleSize", 10)
  private var product: Box[Product] = Empty // private state indicating whether to show product when one is defined
  // and in this context here whether to enable or disable the Recommend button
  // (empty means enable button, Full (product) being present means disable).
  private var confirm = ""

  def render = {
    val toggleButtonsToConsumeJS = Call("lcboViewer.toggleButtonPair", "consume", "recommend")
    val toggleButtonsToRecommendJS = Call("lcboViewer.toggleButtonPair","recommend", "consume")
    def setConfirmJS = SetHtml("confirmMsg", Text(confirm))
    def prodAttributesJS(p: Product) = {
      val nodeSeq = for (x <- p.createProductLIElemVals) yield <li> {x} </li>
      SetHtml("prodAttributes", nodeSeq)
    }
    def prodDisplayJS(prod: Product) =  SetHtml("productImg", <img src={prod.imageThumbUrl}/>) & prodAttributesJS(prod) & JsShowId("prodDisplay")
    val hideProdDisplayJS =  JsHideId("prodDisplay")
    def cancelCbJS = toggleButtonsToRecommendJS & setConfirmJS & hideProdDisplayJS

    def recommend(): JsCmd = {
      def maySelect(): JsCmd =
        TheStore.is.id match {
          // validates expected numeric input TheStore (a http session attribute) and when valid,
          // do real handling of accessing LCBO data
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
            confirm = ""
            prod.dmap {
              Noop
            } { p: Product =>
              product = Full(p)
              S.clearCurrentNotices // clear error message to make way for normal layout representing normal condition.
              prodDisplayJS(p)
            }
          case _ => S.error(s"Enter a number > 0 for Store")
          // Error goes to site menu, but we could also send it to a DOM element if we were to specify an additional parameter
        }

      product match {
        case Full(p) => prodDisplayJS(p)
        // ignore consecutive clicks for flow control, ensuring we take only the user's first click as actionable
        // for a series of clicks on button before we have time to disable it
        case _ => maySelect() & toggleButtonsToConsumeJS & setConfirmJS
        // normal processing  (ProductConsume does it the other way around as it plays opposite role as to when it should be active)
      }
    }

    def consume(): JsCmd = {
      def mayConsume(p: Product): JsCmd = {
        Product.consume(p) match {
          case util.Success((userName, count)) =>
            confirm =s"${p.name} has now been purchased $count time(s), $userName"
            product = Empty
            S.clearCurrentNotices // clears error message now that this is good, to get a clean screen.
          case util.Failure(ex) => S.error(s"Unable to sell you product ${p.name} with error '$ex'")
        }
      }

      product match {
        case Full(p)  => mayConsume(p) & toggleButtonsToRecommendJS & hideProdDisplayJS & setConfirmJS
          // we got notified that we have a product that can be consumed and user expressed interest in consuming.
          // So, try it as a simulation by doing a DB store.
        case _ => Noop // ignore consecutive clicks, ensuring we do not attempt to consume multiple times in a row on a string of clicks from user
      }
    }

    def cancel(): JsCmd = {
      confirm = ""
      product = Empty
      cancelCbJS
    }

    val buttonPairCssSel: CssSel = product match {
      case Full(p) =>
       "#selectConfirmation" #> s"For social time, we suggest you: ${p.name}"  // assigns RHS text message as value of DOM element selectConfirmation
        "li *" #> p.createProductLIElemVals & // generates many html li items on the fly one per list entry.
        ClearClearable // ensures list items appear only once (not duplicated)
      case _ =>
        "li [hidden+]" #> "true"
    }
    val addConsumeImgElem: CssSel = "@consume *+" #> <img src="/images/winesmall.png" alt="consume represented as glass of wine"/>
    val addRecommendImgElem: CssSel = "@recommend *+" #> <img src="/images/recommend.png" alt="recommend represented as question mark"/>
    val addCancelImgElem: CssSel = "@cancel *+" #> <img src="/images/cancel.png" alt="cancel represented as X"/>

    "* [onclick]" #> SHtml.ajaxCall(JsRaw("this.value"), { (s: String) =>
       s match {
         case "consume" => consume()
         case "recommend" => recommend()
         case "cancel" => cancel()
         case _ => Noop
       }
    }) & buttonPairCssSel & addCancelImgElem & addRecommendImgElem & addConsumeImgElem &
    "#confirmMsg *"  #> confirm
 }

}
