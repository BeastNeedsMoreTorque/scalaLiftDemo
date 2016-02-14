package code.snippet

import code.model._
import code.snippet.SessionCache._
import net.liftweb.common._
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.{JE, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.S
import net.liftweb.json
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import net.liftweb.http.SHtml.{ajaxInvoke,ajaxSelect,jsonCall}

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

  case class SelectedProduct(lcbo_id: Int, quantity: Int, cost: Double)

  def formatInDollars(d: Double) = "$" +f"$d%1.2f" // EN lang (FR has it at tail! Shamelessly unconcerned about locale here).

  def render = {
    def transactionsConfirmationJS(user: String, confirmationMsgs: Iterable[(SelectedProduct, String)]) = {
      def getSingleLI(el: (SelectedProduct, String)): NodeSeq = {
        val formattedCost = formatInDollars(el._1.cost)
        val ns: NodeSeq = <li>{el._2} at cost of {formattedCost} for {el._1.quantity} extra units</li>
        ns
      }
      @tailrec
      def getLIs(currNodeSeq: NodeSeq, l: Iterable[(SelectedProduct, String)]): NodeSeq = {
        if (l.isEmpty) return currNodeSeq
        getLIs(currNodeSeq ++ getSingleLI(l.head), l.tail)
      }
      val severalLIs = getLIs(NodeSeq.Empty, confirmationMsgs)
      val totalBill = formatInDollars(confirmationMsgs.map{x => x._1.quantity * x._1.cost}.sum)

      SetHtml("transactionsConfirmationUser", Text(user)) &
      SetHtml("purchaseAmount", Text(totalBill)) &
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
        val checkBoxNS = <label><input type="checkbox" class="prodSelectInput" onclick="prodSelection.updateItem(this);" value={lcbo_id}></input>{name}</label><br></br>
        val quantityNS = <label>Item Quantity:<input type="text" class="prodQty prodSelectInput" name={lcbo_id}></input></label><br></br>
        val costNS = <label>Cost:<input type="text" class="prodCost prodSelectInput" name={lcbo_id} value={prod.price} readonly="readonly"></input></label>
        val hiddenCostNS = <input type="text" class="hiddenProdCost" value={prod.price} hidden="hidden"></input>

        val imgNS = <img src={prod.imageThumbUrl.get}/>
        val ns: NodeSeq =  <div><div class="span-8">{attributesNS}</div><div class="span-8 last">{imgNS}<br></br>{checkBoxNS}{quantityNS}{costNS}{hiddenCostNS}</div></div><hr></hr>
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
          val qtyProdSeq = Store.recommend(theStoreId.is, theCategory.is, theRecommendCount.is) match {
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

    def consume(selectedProds: List[SelectedProduct]) = {
      case class Feedback(userName: String, confirmation: String, error: String)
      def mayConsumeItem(p: Product, quantity: Int): Feedback = {
        UserProduct.consume(p, quantity) match {
          case Full((userName, count)) =>
            Feedback(userName, s"${p.name} $count time(s)", "")
          case Failure(x, ex, _) =>
            Feedback(userName="", confirmation="", error = s"Unable to sell you product ${p.name} with error $x and exception '$ex'")
          case Empty =>
            Feedback(userName="", confirmation="", error = s"Unable to sell you product ${p.name}")
        }
      }

      def mayConsume: JsCmd = {
        val productPairs: List[(SelectedProduct, Option[Product])] = selectedProds.map(p => (p, Product.getProduct(p.lcbo_id)))
        val feedback: List[(SelectedProduct, Feedback)] = for (pair <- productPairs;
                            p <- pair._2;
                            f <- Option(mayConsumeItem(p, pair._1.quantity))) yield(pair._1, f) //products.map(mayConsumeItem)
        feedback.map {pair: (SelectedProduct, Feedback) => pair._2.error}.filter(_.nonEmpty).map(S.error) // show all errors when we attempted to go to DB for user products relationship
        val confirmations = feedback.filter( _._2.confirmation.nonEmpty) // select those for which we have no error and explicit useful message
        if (confirmations.nonEmpty) transactionsConfirmationJS(confirmations.head._2.userName, confirmations.map{pair => (pair._1, pair._2.confirmation) }) & consumeCbJS // confirm and show only if there's something interesting
        else {S.error("could not reconcile product id in cache!"); Noop}
      }

      if (selectedProds.nonEmpty) mayConsume // Normal case: we got notified that we have a product that can be consumed and user expressed interest in consuming.
      else { S.notice("consume", "Select some recommended products before attempting to consume"); Noop}
    }

    def cancel = {
      S.error("")
      cancelCbJS
    }

    def consumeProducts(j: JValue): JsCmd = {
      val jsonOpt = j.extractOpt[String].map( parse)
      val selectedProducts: Option[List[SelectedProduct]] = jsonOpt.map( json => {for (p <- json.children) yield p.extractOpt[SelectedProduct]}.flatten )
      consume(selectedProducts.fold(List[SelectedProduct]())(identity))
    }

    val recommendCountValues = Map[String, Int](
      "1" -> 1,
      "5" -> 5,
      "10" -> 10,
      "20" -> 20
    )
    val recommendCountDefault = Full("1")
    val recommendCountOptions : List[(String,String)] = recommendCountValues.keys.map(p => (p,p)).toList

    // call to setBorderJS after button activation simply highlights that button was pressed.
    "@consume [onclick]" #>
      jsonCall( JE.Call("prodSelection.currentProds"),
                      { j: JValue => consumeProducts(j) & setBorderJS("consume")}) & // fetch in JSON with JS Call the lcbo product IDs and then deal with them
    "@recommend [onclick]" #>
      ajaxInvoke({() => recommend & setBorderJS("recommend")}) &
    "@cancel [onclick]" #>
      ajaxInvoke({() => cancel & setBorderJS("cancel")}) &
    "select" #> ajaxSelect(recommendCountOptions, recommendCountDefault,
      { selected: String => theRecommendCount.set(toInt(selected)); Noop })

  }
}
