package code.snippet

import code.model._
import code.snippet.SessionCache._
import net.liftweb.common._
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.{JE, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, SHtml}
import net.liftweb.json
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import scala.annotation.tailrec
import scala.xml._
import scala.language.implicitConversions

/**
  * This snippet contains state only via HTTP session (A Liftweb snippet is a web page fragment that has autonomous existence
  * equivalent to a page with framework
  * responsible to render all snippets on a page and to handle events between snippets and corresponding html div elements)
  * Much html and Javascript is generated here thanks to the capabilities of liftweb.
  * Created by philippederome on 15-10-26.
  */
object ProductInteraction extends Loggable {
  private implicit val formats = net.liftweb.json.DefaultFormats

  val prodsPerPageToDisplay = Math.max(1,Props.getInt("product.prodsPerPageToDisplay", 5)) // it'd be very unconscious to choose less than 1.
  private val interactionsToImgMap: Map[String, String] = {
    val interactionsToImgMapAsStr = Props.get("product.interactionsImageMap", "") // get it in JSON format
    val jval: json.JValue = parse(interactionsToImgMapAsStr) // contains list of JField(String, JString(String))
    val v = for (elem <- jval.children.toVector) yield elem.values // contains vector of (String, String)
    v.map(_.asInstanceOf[(String, String)]).toMap  // could throw if contents that are configured are in wrong format (violating assumption of pairs (String,String)...
  }

  private val radioOptions: Seq[RadioElements] = RadioElements.radioOptions(  List("recommend", "cancel"), "cancel", interactionsToImgMap)

  private val hideProdDisplayJS =  JsHideId("prodDisplay")
  def setBorderJS(elt: String) = Call("toggleImage.frameRadioImage", "prodInteractionContainer", {elt})

  def render = {
    def transactionsConfirmationJS(user: String, confirmationMsgs: Iterable[String]) = {
      def getSingleLI(el: String): NodeSeq = {
        val ns: NodeSeq = <li>{el}</li>
        ns
      }
      @tailrec
      def getLIs(currNodeSeq: NodeSeq, l: Iterable[String]): NodeSeq = {
        if (l.isEmpty) return currNodeSeq
        getLIs(currNodeSeq ++ getSingleLI(l.head), l.tail)
      }
      val severalLIs = getLIs(NodeSeq.Empty, confirmationMsgs)

      SetHtml("transactionsConfirmationUser", Text(user)) &
      SetHtml("transactionsConfirmation", severalLIs) &
      JsShowId("confirmationDiv")
    }

    def prodDisplayJS(qtyProds: Iterable[(Int, Product)]) = {
      def getSingleDiv(el: (Int, Product)): NodeSeq = {
        // create a checkBox with value being product lcbo_id (key for lookups) and label's html representing name. The checkbox state is picked up when we call JS in this class
        val checkBoxNodeSeq = <label><input type="checkbox" class="productCB" name="product" value={el._2.lcbo_id.get.toString}></input>{el._2.name.get}</label>
        val attributesNodeSeq = <table>{for (x <-  el._2.createProductElemVals ++ List(("Quantity: ", el._1 )) ) yield <tr><td class="prodAttrHead">{x._1}</td><td class="prodAttrContent">{x._2}</td></tr>}</table>
        val imgNodeSeq = <img src={el._2.imageThumbUrl.get}/>
        val ns: NodeSeq =  <div><div class="span-8">{attributesNodeSeq}{checkBoxNodeSeq}</div><div class="span-8 last">{imgNodeSeq}</div></div><hr></hr>
        ns
      }

      @tailrec
      def getDivs(currNodeSeq: NodeSeq, l: Iterable[(Int, Product)]): NodeSeq = {
        if (l.isEmpty) return currNodeSeq
        getDivs(currNodeSeq ++ getSingleDiv(l.head), l.tail)
      }

      val severalDivs = getDivs(NodeSeq.Empty, qtyProds)
      SetHtml("prodContainer", severalDivs) &
      JsHideId("confirmationDiv") &
      JsShowId("prodDisplay")
    }

    // Following 3 values are JavaScript objects to be executed when returning from Ajax call cb to browser to execute on browser
    // for the 3 events corresponding to the 3 buttons (for normal cases when there are no errors). We need to execute strictly Scala callbacks
    // here before invoking these JS callbacks. lazy val or def is required here because the value of Session variables changes as we handle events.
    def cancelCbJS =  hideProdDisplayJS & JsHideId("confirmationDiv") // transactionsConfirmationJS

    def consumeCbJS = hideProdDisplayJS    // meant to simulate consumption of product, or possibly a commentary on one

    def recommend() = {
      def maySelect(): JsCmd =
        if (theStoreId.is > 0 ) {
          // validates expected numeric input TheStore (a http session attribute) and when valid,
          // do real handling of accessing LCBO data
          val prodInvSeq = Store.recommend(theStoreId.is, theCategory.is, prodsPerPageToDisplay) match {
            // we want to distinguish error messages to user to provide better diagnostics.
            case Full(pairs) => Full(pairs) // returns prod and quantity in inventory normally
            case Failure(m, ex, _) => S.error(s"Unable to choose product of category ${theCategory.is} with message $m and exception error $ex"); Empty
            case Empty => S.error(s"Unable to choose product of category ${theCategory.is}"); Empty
          }
          prodInvSeq.dmap { Noop }
          { qtyProds =>
            S.error("") // work around clearCurrentNotices clear error message to make way for normal layout representing normal condition.
            prodDisplayJS( qtyProds)
          }
        }
        else {
          S.error(s"We need to establish your local store first, please wait for local geo to be available or enter a postal code")
          Noop     // Error goes to site menu, but we could also send it to a DOM element if we were to specify an additional parameter
        }

      User.currentUser.dmap { S.error("recommend", "recommend feature unavailable, Login first!"); Noop }
      { user => maySelect()} // normal processing
    }

    def consume(lcbo_ids: List[Int]) = {
      case class Feedback(userName: String, confirmation: String, error: String)
      def mayConsumeItem(p: Product): Feedback = {
        UserProduct.consume(p) match {
          case Full((userName, count)) =>
            Feedback(userName, s"${p.name} $count time(s)", "")
          case Failure(x, ex, _) =>
            Feedback(userName="", confirmation="", error = s"Unable to sell you product ${p.name} with error $x and exception '$ex'")
          case Empty =>
            Feedback(userName="", confirmation="", error = s"Unable to sell you product ${p.name}")
        }
      }

      def mayConsume: JsCmd = {
        val products = lcbo_ids.flatten(Product.getProduct (_))
        val feedback = products.map(mayConsumeItem _)
        feedback.map( _.error).filter(!_.isEmpty).map(S.error(_)) // show all errors when we attempted to go to DB for user products relationship
        val confirmations = feedback.filter( !_.confirmation.isEmpty) // select those for which we have no error and explicit useful message
        if (!confirmations.isEmpty) transactionsConfirmationJS(confirmations.head.userName, confirmations.map(_.confirmation)) // confirm and show only if there's something interesting
        else Noop
      }

      // ignore consecutive clicks (when theRecommendedProducts is defined), ensuring we do not attempt to consume multiple times in a row on a string of clicks from user
      if (lcbo_ids.isEmpty) { S.notice("consume", "Select some recommended products before attempting to consume"); Noop}
      else  mayConsume & consumeCbJS  // Normal case: we got notified that we have a product that can be consumed and user expressed interest in consuming.
    }

    def cancel() = {
      S.error("")
      cancelCbJS
    }

    def consumeProducts(j: JValue): JsCmd = {
      val jsonOpt = j.extractOpt[String].map( parse(_))
      val lcboIdsSeq: Option[List[Int]] = jsonOpt.map( json => {for (p <- json.children)  yield p.extract[Int]  } )
      consume(lcboIdsSeq.fold(List[Int]())(identity)) & setBorderJS("consume")
    }

    "@consume [onclick]" #>
      SHtml.jsonCall( JE.Call("prodSelection.currentProds"), consumeProducts _ ) &
    ".options" #> LabelStyle.toForm( SHtml.ajaxRadio( radioOptions, Empty,
      (choice: RadioElements) => {
        choice.name match {
          case "recommend" => recommend() & setBorderJS(choice.name)
          case "cancel" => cancel() & setBorderJS(choice.name)
          case _ => Noop // for safety
        }
      }))
  }
}
