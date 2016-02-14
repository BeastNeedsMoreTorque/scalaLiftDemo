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
  private val showProdDisplayJS =  JsShowId("prodDisplay")
  private val showConfirmationJS =  JsShowId("confirmationDiv")
  private val hideConfirmationJS =  JsHideId("confirmationDiv")

  def setBorderJS(elt: String) = Call("toggleButton.frame", "prodInteractionContainer", {elt})

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
      showConfirmationJS
    }

    def prodDisplayJS(qtyProds: Iterable[(Int, Product)]) = {
      def getSingleDiv(el: (Int, Product)): NodeSeq = {
        val quantity = el._1
        val prod = el._2
        val lcbo_id = prod.lcbo_id.toString
        val name = prod.name.get
        val attributesNS = <table>{for (x <-  prod.createProductElemVals ++ List(("Quantity: ", quantity)) ) yield <tr><td class="prodAttrHead">{x._1}</td><td class="prodAttrContent">{x._2}</td></tr>}</table>

        // create a checkBox with value being product lcbo_id (key for lookups) and label's html representing name. The checkbox state is picked up when we call JS in this class
        val checkBoxNS = <label><input type="checkbox" onclick="prodSelection.updateItem(this);" value={lcbo_id}></input>{name}</label><br></br>
        val quantityNS = <label>Item Quantity:<input type="text" class="prodQty" name={lcbo_id}></input></label><br></br>
        val costNS = <label>Cost:<input type="text" class="prodCost" name={lcbo_id} value={prod.price}></input></label>
        val hiddenCostNS = <input type="text" class="hiddenProdCost" value={prod.price} hidden="hidden"></input>

        val imgNS = <img src={prod.imageThumbUrl.get}/>
        val ns: NodeSeq =  <div><div class="span-8">{attributesNS}{checkBoxNS}{quantityNS}{costNS}{hiddenCostNS}</div><div class="span-8 last">{imgNS}</div></div><hr></hr>
        ns
      }

      @tailrec
      def getDivs(currNodeSeq: NodeSeq, l: Iterable[(Int, Product)]): NodeSeq = {
        if (l.isEmpty) return currNodeSeq
        getDivs(currNodeSeq ++ getSingleDiv(l.head), l.tail)
      }

      val severalDivs = getDivs(NodeSeq.Empty, qtyProds)
      SetHtml("prodContainer", severalDivs) & hideConfirmationJS & showProdDisplayJS
    }

    // Following 3 values are JavaScript objects to be executed when returning from Ajax call cb to browser to execute on browser
    // for the 3 events corresponding to the 3 buttons (for normal cases when there are no errors). We need to execute strictly Scala callbacks
    // here before invoking these JS callbacks. lazy val or def is required here because the value of Session variables changes as we handle events.
    def cancelCbJS =  hideProdDisplayJS & hideConfirmationJS

    def consumeCbJS = hideProdDisplayJS & showConfirmationJS   // meant to simulate consumption of product, or possibly a commentary on one

    def recommend = {
      def maySelect(): JsCmd =
        if (theStoreId.is > 0 ) {
          // validates expected numeric input TheStore (a http session attribute) and when valid,
          // do real handling of accessing LCBO data
          val qtyProdSeq = Store.recommend(theStoreId.is, theCategory.is, prodsPerPageToDisplay) match {
            // we want to distinguish error messages to user to provide better diagnostics.
            case Full(pairs) => Full(pairs) // returns prod and quantity in inventory normally
            case Failure(m, ex, _) => S.error(s"Unable to choose product of category ${theCategory.is} with message $m and exception error $ex"); Empty
            case Empty => S.error(s"Unable to choose product of category ${theCategory.is}"); Empty
          }
          qtyProdSeq.dmap { Noop }
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
        val products = lcbo_ids.flatten(Product.getProduct)
        val feedback = products.map(mayConsumeItem)
        feedback.map( _.error).filter(_.nonEmpty).map(S.error) // show all errors when we attempted to go to DB for user products relationship
        val confirmations = feedback.filter( _.confirmation.nonEmpty) // select those for which we have no error and explicit useful message
        if (confirmations.nonEmpty) transactionsConfirmationJS(confirmations.head.userName, confirmations.map(_.confirmation)) & consumeCbJS // confirm and show only if there's something interesting
        else {S.error("could not reconcile product id in cache!"); Noop}
      }

      // ignore consecutive clicks (when theRecommendedProducts is defined), ensuring we do not attempt to consume multiple times in a row on a string of clicks from user
      if (lcbo_ids.isEmpty) { S.notice("consume", "Select some recommended products before attempting to consume"); Noop}
      else mayConsume // Normal case: we got notified that we have a product that can be consumed and user expressed interest in consuming.
    }

    def cancel = {
      S.error("")
      cancelCbJS
    }

    def consumeProducts(j: JValue): JsCmd = {
      val jsonOpt = j.extractOpt[String].map( parse)
      val lcboIdsSeq: Option[List[Int]] = jsonOpt.map( json => {for (p <- json.children) yield p.extractOpt[Int]}.flatten )
      consume(lcboIdsSeq.fold(List[Int]())(identity))
    }

    // call to setBorderJS after button activation simply highlights that button was pressed.
    "@consume [onclick]" #>
      SHtml.jsonCall( JE.Call("prodSelection.currentProds"),
                      { j: JValue => consumeProducts(j) & setBorderJS("consume")}) & // fetch in JSON with JS Call the lcbo product IDs and then deal with them
    "@recommend [onclick]" #>
      SHtml.ajaxInvoke({() => recommend & setBorderJS("recommend")}) &
    "@cancel [onclick]" #>
      SHtml.ajaxInvoke({() => cancel & setBorderJS("cancel")})
  }
}
