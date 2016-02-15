package code.snippet

import code.model._
import code.snippet.SessionCache._
import net.liftweb.common._
import net.liftweb.http.js.{JE, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.S
import net.liftweb.json.JsonParser.parse
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml.{ajaxInvoke,ajaxSelect,jsonCall}

import scala.xml._
import scala.language.implicitConversions

import java.text.NumberFormat

/**
  * This snippet contains state only via HTTP session (A Liftweb snippet is a web page fragment that has autonomous existence
  * equivalent to a page with framework
  * responsible to render all snippets on a page and to handle events between snippets and corresponding html div elements)
  * Much html and Javascript is generated here thanks to the capabilities of liftweb.
  * Created by philippederome on 15-10-26.
  */
object ProductInteraction extends Loggable {
  case class Feedback(userName: String, success: Boolean, message: String) // outcome of userName's selection of a product, message is confirmation when successful, error when not
  case class QuantityOfProduct(quantity: Int, product: Product)  // quantity available at the current store for a product (store is implied by context)
  case class SelectedProduct(lcbo_id: Int, quantity: Int, cost: Double) // to capture user input via JS and JSON
  case class PurchasedProductConfirmation(selectedProduct: SelectedProduct, confirmation: String)

  private implicit val formats = net.liftweb.json.DefaultFormats

  private val hideProdDisplayJS =  JsHideId("prodDisplay")
  private val showProdDisplayJS =  JsShowId("prodDisplay")
  private val showConfirmationJS =  JsShowId("confirmationDiv")
  private val hideConfirmationJS =  JsHideId("confirmationDiv")

  val formatter = NumberFormat.getCurrencyInstance()

  // data store for a count of products select menu, could easily be generalized to be configured from properties
  object RecommendCount {
    val values = Map[String, Int](
      "1" -> 1,
      "5" -> 5,
      "10" -> 10,
      "20" -> 20
    )
    val default = Full("1")
    val options: List[(String, String)] = values.keys.map(p => (p, p)).toList
  }

  def render = {
    def recommend: JsCmd = {
      def prodDisplayJS(qOfProds: Iterable[QuantityOfProduct]): JsCmd = {
        def getDiv(qOfProd: QuantityOfProduct): NodeSeq = {
          // layout description:
          // attributes in center column (Blueprint class span-8, width 8 not to side) and to its right (Blueprint classes span-8 last) other data.
          // Other data is top to bottom: image, then checkbox, quantity and cost input text fields right justified (CSS class prodSelectInput).
          // We also add a hiddenCost, so that the cost per item is always available for our calculation (visible to user in attributes in any event, but harder for us to get at for computation)
          val quantity = qOfProd.quantity
          val prod = qOfProd.product
          val quantityAttribute = ProductAttribute("Quantity:", quantity.toString)
          val fullAttributesList = prod.createProductElemVals ++ List(quantityAttribute)
          val attributesNS = <table>
            { for (attr <- fullAttributesList )
              yield <tr>
                <td class="prodAttrHead">{attr.key}</td>
                <td class="prodAttrContent">{attr.value}</td>
              </tr>
            }
          </table>

          val lcbo_id = prod.lcbo_id.toString
          val name = prod.name.get
          // create a checkBox with value being product lcbo_id (key for lookups) and label's html representing name. The checkbox state is picked up when we call JS in this class
          val checkBoxNS = <label>
            <input type="checkbox" class="prodSelectInput" onclick="prodSelection.updateItem(this);" value={lcbo_id}/>
            {name}
          </label><br/>
          val quantityNS = <label>Item Quantity:
            <input type="text" class="prodQty prodSelectInput"/>
          </label><br/>
          // read-only costNS, so user can see it clearly but cannot update it.
          val costNS = <label>Cost:
            <input type="text" class="prodCost prodSelectInput" value={prod.price} readonly="readonly"/>
          </label>
          val hiddenCostNS = <input type="text" class="hiddenProdCost" value={prod.price} hidden="hidden"/>

          val imgNS = <img src={prod.imageThumbUrl.get}/>
          val ns: NodeSeq =  <div>
            <div class="span-8">{attributesNS}</div>
            <div class="span-8 last">{imgNS}<br></br>{checkBoxNS}{quantityNS}{costNS}{hiddenCostNS}</div>
          </div><hr/>
          ns
        }

        // for each element of qOfProds, build a Div as NodeSeq and concatenate them as a NodeSeq for the several divs
        val divs = qOfProds.map(getDiv).foldLeft(NodeSeq.Empty)((a: NodeSeq, b: NodeSeq) => (a ++ b))
        SetHtml("prodContainer", divs) & hideConfirmationJS & showProdDisplayJS  // JsCmd (JavaScript  (n JsCmd) can be chained as bigger JavaScript command)
      }

      def maySelect: JsCmd =
        if (theStoreId.is > 0 ) {
          // validate expected numeric input theStoreId (a http session attribute) then access LCBO data
          val quantityProdSeq = Store.recommend(theStoreId.is, theCategory.is, theRecommendCount.is) match {
            // we want to distinguish error messages to user to provide better diagnostics.
            case Full(pairs) => Full(pairs) // returns prod and quantity in inventory normally
            case Failure(m, ex, _) => S.error(s"Unable to choose product of category ${theCategory.is} with message $m and exception error $ex"); Empty
            case Empty => S.error(s"Unable to choose product of category ${theCategory.is}"); Empty
          }
          quantityProdSeq.dmap { Noop } // we gave notice of error already via JS, nothing else to do
          { pairs => // normal case
            S.error("") // work around clearCurrentNotices clear error message to make way for normal layout representing normal condition.
            prodDisplayJS( pairs.map{case (quantity, p) => QuantityOfProduct(quantity, p)})
          }
        }
        else {
          S.error("We need to establish your local store first, please wait for local geo to become available")
          Noop
        }

      User.currentUser.dmap { S.error("recommend", "recommend feature unavailable, Login first!"); Noop }
      { user => maySelect} // normal processing
    }

    def consumeProducts(selection: JValue): JsCmd = {
      def transactionsConfirmationJS(user: String, confirmationMsgs: Iterable[PurchasedProductConfirmation]): JsCmd = {
        def getListItem(item: PurchasedProductConfirmation): NodeSeq = {
          def purchaseConfirmationMessage(confirmation: String, formattedCost: String, quantity: Int): String =
            s"${confirmation} including the cost of today's purchase at ${formattedCost} for ${quantity} extra units"

          val formattedCost = formatter format item.selectedProduct.cost
          val liContent = purchaseConfirmationMessage(item.confirmation, formattedCost, item.selectedProduct.quantity)
          val ns: NodeSeq = <li>{liContent}</li>
          ns
        }

        val totalCost = confirmationMsgs.map{ _.selectedProduct.cost}.sum
        val formattedTotalCost = formatter.format(totalCost)
        val listItems = confirmationMsgs.map(getListItem).foldLeft(NodeSeq.Empty)((a: NodeSeq, b: NodeSeq) => (a ++ b))

        SetHtml("transactionsConfirmationUser", Text(user)) &
        SetHtml("purchaseAmount", Text(formattedTotalCost)) &
        SetHtml("transactionsConfirmation", listItems) &
        showConfirmationJS
      }

      def mayConsume(selectedProds: List[SelectedProduct]): JsCmd = {
        def mayConsumeItem(p: Product, quantity: Int): Option[Feedback] = {
          val x = UserProduct.consume(p, quantity) match {
            case Full((userName, count)) =>
              Feedback(userName, success=true, s"${p.name} $count unit(s) over the years")
            case Failure(x, ex, _) =>
              Feedback(userName="", success=false, s"Unable to sell you product ${p.name} with error $x and exception '$ex'")
            case Empty =>
              Feedback(userName="", success=false, s"Unable to sell you product ${p.name}")
          }
          Option(x)
        }

        // associate primitive browser product details for selected products (SelectedProduct) with full data of same products we should have in cache as pairs
        val feedback: List[(SelectedProduct, Feedback)] = for (sp <- selectedProds;
                                                               product <- Product.getProduct(sp.lcbo_id);
                                                               f <- mayConsumeItem(product, sp.quantity)) yield(sp, f)
        val partition = feedback.groupBy(_._2.success) // splits into errors (false success) and normal confirmations (true success) as a map keyed by Booleans possibly of size 0, 1 (not 2)
        partition.get(false).map( _.map{ _._2.message}.map(S.error)) // open the Option for false lookup in map, which gives us list of erroneous feedback, then pump the message into S.error
        val goodConfirmations = partition.get(true) // select those for which we have success and positive message
        goodConfirmations.fold {
          S.error("could not reconcile any selected product id in cache!") // they all failed!
          Noop
        } { goodList: List[(SelectedProduct, Feedback)] =>
            val confirmationMessages =
              goodList.map{case(selectedProd, feedBck) =>
                               PurchasedProductConfirmation(selectedProd, feedBck.message) } // get some particulars about cost and quantity in addition to message
            transactionsConfirmationJS(goodList.head._2.userName, confirmationMessages) &
            hideProdDisplayJS & showConfirmationJS   // meant to simulate consumption of products
        }
      }

      val jsonOpt = selection.extractOpt[String].map( parse)
      val selectedProducts = jsonOpt.map( json => {for (p <- json.children) yield p.extractOpt[SelectedProduct]}.flatten )
      selectedProducts.fold{
        S.warning("Select some recommended products before attempting to consume")
        Noop
      }{
        mayConsume
      }
    }

    def cancel: JsCmd = {
      S.error("")
      hideProdDisplayJS & hideConfirmationJS
    }
    val actionButtonsContainer = "prodInteractionContainer"

    // call to setBorderJS after button activation simply highlights that button was pressed.
    "@consume [onclick]" #>
      jsonCall( JE.Call("prodSelection.currentProds"),
                      { j: JValue => consumeProducts(j) & JSUtilities.setBorderJS(actionButtonsContainer, "consume")}) & // fetch in JSON with JS Call the lcbo product IDs and then deal with them
    "@recommend [onclick]" #>
      ajaxInvoke({() => recommend & JSUtilities.setBorderJS(actionButtonsContainer, "recommend")}) &
    "@cancel [onclick]" #>
      ajaxInvoke({() => cancel & JSUtilities.setBorderJS(actionButtonsContainer, "cancel")}) &
    "select" #> ajaxSelect(RecommendCount.options, RecommendCount.default,
      { selected: String => theRecommendCount.set(toInt(selected)); Noop })
  }
}
