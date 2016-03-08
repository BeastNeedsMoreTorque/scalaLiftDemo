package code.snippet

import code.model._
import code.snippet.SessionCache._
import net.liftweb.common._
import net.liftweb.http.js.{JE, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.S
import net.liftweb.json.JsonParser._
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._

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
  case class QuantityOfProduct(quantity: Long, product: Product)  // quantity available at the current store for a product (store is implied by context)
  case class SelectedProduct(id: Int, quantity: Long, cost: Double) // to capture user input via JS and JSON
  case class SelectedProductFeedback(selectedProduct: SelectedProduct, feedback: Feedback)
  case class PurchasedProductConfirmation(selectedProduct: SelectedProduct, confirmation: String)

  private implicit val formats = net.liftweb.json.DefaultFormats

  private val hideProdDisplayJS =  JsHideId("prodDisplay")
  private val fetchInventoriesJS = JE.Call("inventory.fetchInventories") // let the client deal with incomplete inventories and get them himself
  private val showProdDisplayJS =  JsShowId("prodDisplay") & fetchInventoriesJS
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
    val options = values.keys.map(p => (p, p)).toList
  }

  def render = {
    def recommend(jsStore: JValue): JsCmd = {
      def prodDisplayJS(qOfProds: Iterable[QuantityOfProduct]): JsCmd = {
        def getDiv(qOfProd: QuantityOfProduct): NodeSeq = {
          // layout description:
          // attributes in center column (Blueprint class span-8, width 8 not to side) and to its right (Blueprint classes span-8 last) other data.
          // Other data is top to bottom: image, then checkbox, quantity and cost input text fields right justified (CSS class prodSelectInput).
          // We also add a hiddenCost, so that the cost per item is always available for our calculation (visible to user in attributes in any event, but harder for us to get at for computation)
          val quantity = qOfProd.quantity
          val prod = qOfProd.product
          val id = prod.lcbo_id.get.toString
          val quantityAttribute = code.model.Attribute("Quantity:", quantity.toString)
          val fullAttributes = prod.createProductElemVals ++ Vector(quantityAttribute)
          def setProdIdName(attr: code.model.Attribute) =
            if (attr == quantityAttribute) id else ""

          val attributesNS = <table>
            { for (attr <- fullAttributes )
              yield <tr>
                <td class="prodAttrHead">{attr.key}</td>
                <td class="prodAttrContent" name={setProdIdName(attr)}>{attr.value}</td>
              </tr>
            }
          </table>

          val name = prod.name.get
          // create a checkBox with value being product id (key for lookups) and label's html representing name. The checkbox state is picked up when we call JS in this class
          val checkBoxNS = <label>
            <input type="checkbox" class="prodSelectInput" value={id}/>
            {name}
          </label><br/>
          val quantityNS = <label>Item Quantity:
            <input type="text" class="prodQty prodSelectInput" onchange="prodSelection.updateQtyItem(this);" value="1"/>
          </label><br/>
          // read-only costNS, so user can see it clearly but cannot update it.
          val costNS = <label>Cost:
            <input type="text" class="prodCost prodSelectInput" value={prod.price} readonly="readonly"/>
          </label>
          val hiddenCostNS = <input type="text" class="hiddenProdCost" value={prod.price} hidden="hidden"/>

          val imgNS = <img src={prod.imageThumbUrl}/>
          val ns: NodeSeq =  <div>
            <div class="span-8">{attributesNS}</div>
            <div class="span-8 last">{imgNS}<br></br>{checkBoxNS}{quantityNS}{costNS}{hiddenCostNS}</div>
          </div><hr/>
          ns
        }

        // for each element of qOfProds, build a Div as NodeSeq and concatenate them as a NodeSeq for the several divs
        val divs = qOfProds.map(getDiv).fold(NodeSeq.Empty)((a: NodeSeq, b: NodeSeq) => a ++ b)
        SetHtml("prodContainer", divs) & hideConfirmationJS & showProdDisplayJS  // JsCmd (JavaScript  (n JsCmd) can be chained as bigger JavaScript command)
      }

      def maySelect(lcbo_storeId: Int): JsCmd =
        Store.getStoreByLcboId(lcbo_storeId).fold {
          S.error("We need to establish your local store first, please wait for local geo to become available")
          Noop
        }{ s: Store =>
          // validate expected numeric input storeId then access LCBO data
          val quantityProdSeq = s.recommend(theCategory.is, theRecommendCount.is) match {
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

      val json = jsStore.extractOpt[String].map( parse)
      val lcbo_storeId = json.fold(-1){ json =>  json.extract[Int]}
      User.currentUser.dmap { S.error("recommend", "recommend feature unavailable, Login first!"); Noop }
      { user => maySelect(lcbo_storeId)} // normal processing
    }

    def consumeProducts(selection: JValue): JsCmd = {
      def transactionsConfirmationJS(user: String, confirmationMsgs: Iterable[PurchasedProductConfirmation]): JsCmd = {
        def getItem(item: PurchasedProductConfirmation): NodeSeq = {
          def purchaseConfirmationMessage(confirmation: String, formattedCost: String, quantity: Long): String =
            s"$confirmation including the cost of today's purchase at $formattedCost for $quantity extra units"

          val formattedCost = formatter format item.selectedProduct.cost
          val liContent = purchaseConfirmationMessage(item.confirmation, formattedCost, item.selectedProduct.quantity)
          <li>{liContent}</li> :NodeSeq
        }

        val totalCost = confirmationMsgs.map{ _.selectedProduct.cost}.sum
        val formattedTotalCost = formatter.format(totalCost)
        val listItems = confirmationMsgs.map(getItem).fold(NodeSeq.Empty)( (a, b) => a ++ b )
        SetHtml("transactionsConfirmationUser", Text(user)) &
        SetHtml("purchaseAmount", Text(formattedTotalCost)) &
        SetHtml("transactionsConfirmation", listItems) &
        showConfirmationJS
      }

      def mayConsume(selectedProds: IndexedSeq[SelectedProduct]): JsCmd = {
        def mayConsumeItem(p: Product, quantity: Long): Feedback = {
          User.currentUser.dmap { Feedback(userName="", success=false, "unable to process transaction, Login first!") }
          { user =>
            user.consume(p, quantity) match {
              case Full((userName, count)) => // the good
                Feedback(userName, success = true, s"${p.name} $count unit(s) over the years")
              case Failure(e, ex, _) =>  // the bad
                Feedback(userName = "", success = false, s"Unable to sell you product ${p.name} with error $e and exception '$ex'")
              case Empty =>  // the ugly
                Feedback(userName = "", success = false, s"Unable to sell you product ${p.name}")
            }
          }
        }

        // associate primitive browser product details for selected products (SelectedProduct) with full data of same products we should have in cache as pairs
        val feedback =
          for(sp <- selectedProds;
              product <- Product.getProductByLcboId(sp.id);
              f = mayConsumeItem(product, sp.quantity)) yield SelectedProductFeedback(sp, f)
        val goodAndBackFeedback = feedback.groupBy(_.feedback.success) // splits into errors (false success) and normal confirmations (true success) as a map keyed by Booleans possibly of size 0, 1 (not 2)
        goodAndBackFeedback.getOrElse(false, Nil).map( _.feedback.message).foreach(S.error) // open the Option for false lookup in map, which gives us list of erroneous feedback, then pump the message into S.error
        val goodFeedback = goodAndBackFeedback.getOrElse(true, Nil) // select those for which we have success and positive message
        if (goodFeedback.isEmpty) {
          S.error("Make sure you select a product if you want to consume") // they all failed!
          Noop
        }
        else {
          if (goodFeedback.size == feedback.size) S.error("") // no error, erase old errors no longer applicable.
          val confirmationMessages = goodFeedback.map{ x => PurchasedProductConfirmation(x.selectedProduct, x.feedback.message) } // get some particulars about cost and quantity in addition to message
          val userName = goodFeedback.head.feedback.userName
          transactionsConfirmationJS(userName, confirmationMessages) &
          hideProdDisplayJS & showConfirmationJS   // meant to simulate consumption of products
        }
      }

      val jsonOpt = selection.extractOpt[String].map( parse)
      val selectedProducts: Option[Vector[SelectedProduct]] = jsonOpt.map(json => {for (p <- json.children.toVector) yield p.extractOpt[SelectedProduct]}.flatten )
      selectedProducts.fold {
        S.warning("Select some recommended products before attempting to consume")
        Noop
      } { mayConsume }
    }

    def cancel: JsCmd = {
      S.error("")
      hideProdDisplayJS & hideConfirmationJS
    }
    val actionButtonsContainer = "prodInteractionContainer"

    theRecommendCount.set(toInt(RecommendCount.default))
    // call to setBorderJS after button activation simply highlights that button was pressed.
    "@consume [onclick]" #>
      jsonCall( JE.Call("prodSelection.currentProds"),
                      { j: JValue => consumeProducts(j) & JSUtilities.setBorderJS(actionButtonsContainer, "consume")}) & // fetch in JSON with JS Call the lcbo product IDs and then deal with them
    "@cancel [onclick]" #>
      ajaxInvoke({() => cancel & JSUtilities.setBorderJS(actionButtonsContainer, "cancel")}) &
    "select" #> ajaxSelect(RecommendCount.options, RecommendCount.default,
      { selected: String => theRecommendCount.set(toInt(selected)); Noop }) andThen // always before recommend so it takes effect.
    "@recommend [onclick]" #>
      jsonCall( JE.Call("storeFinder.getTheSelectedStore"),
        { j: JValue => recommend(j) & JSUtilities.setBorderJS(actionButtonsContainer, "recommend")})
  }
}
