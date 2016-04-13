package code.snippet

import java.text.NumberFormat

import scala.xml._
import scala.language.implicitConversions
import net.liftweb.common._
import net.liftweb.http.js.{JE, JsCmd}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.S
import net.liftweb.json.JsonParser._
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._

import code.model.{IProduct,Product,Store,User}
import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.snippet.SessionCache._

/**
  * This is a Lift Snippet: it plays the equivalent of a controller and a view with render being responsible to render to client (a bit like a PLAY! Action)
  * Lift embraces statefulness while Play eschews it. So we have state in HTTP session (not in cookie).
  *
  * Lift uses JS in request chain at start of event (see consume making use of jsonCall), then it does its thing on server side, much like a Controller.
  * Then it executes a JS command, which effects the browser; that JS command can be a chain of JS commands and we use that heavily.
  * This means much JS control actually takes place here.
  * Snippet does not use REST. REST API is used elsewhere in app and could be used more. Comets is available in Lift and apparently compares favourably with Akka technology. I don't use it.
  * HTTP request chain is asynchronous from browser perspective (no page reload); however the render method is synchronous. For full asynchronicity from web to browser,
  * comet is the available technology (so this is not like standard PLAY Action).
  *
  * Snippets are doing transformation of XML nodes in the document and is DEFINITELY not HTML page centric. Snippets can embed each other on a html page to reduce markup duplication.
  * The snippet transformation of rewriting a XML node is done at the end of the render function matching elements in CSS style and using some LIFT DSL ( #> )
  * to mean "rewrite".
  * The various elements that are rewritten occur in parallel unless parallelism is turned off with "andThen" as per other Scala idioms. The parallel chaining is done with &.
  *
  * Here we do 4 rewrites, one for the ? (recommend) button, the x (cancel) button, the glass (consume) button and the recommend count drop down select menu.
  * The select menu of # items is always evaluated serially before the recommend button callback is activated so that we know properly the right number of products to recommend.
  *
  * The prodDisplyJS and getDivs function below can be thought as an Action callback that mixes up markup and Scala. The structure deliberately follows
  * the markup of index.html. There's no template engine in Lift, the html pages are pure html and contain no Scala code. The design view seems to be that Scala developers
  * can build everything in the stack and they don't want to get into JS too often; also the no-template approach is apparently meant to prevent web designers to check in changes to markup
  * that break business logic (in Scala code) by accident by not letting them near the Scala code.
  *
  * This snippet contains state only via HTTP session (A Liftweb snippet is a web page fragment that has autonomous existence
  * equivalent to a page with lift framework
  * being responsible to render all snippets on a page and to handle events between snippets and corresponding html div elements)
  * Much html and Javascript is generated here thanks to the capabilities of liftweb.
  * Created by philippederome on 15-10-26.
  */
class ProductInteraction extends JSUtilities with Loggable {
  case class Feedback(userName: String, success: Boolean, message: String) // outcome of userName's selection of a product, message is confirmation when successful, error when not
  case class QuantityOfProduct(quantity: Long, product: IProduct)  // quantity available at the current store for a product (store is implied by context)
  case class SelectedProduct(id: Long, quantity: Long, cost: Double, missedQty: Long) // to capture user input via JS and JSON (stick to Long to simplify interface with JS)
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
          import code.model.Attribute
          // layout description:
          // attributes in center column (Blueprint class span-8, width 8 not to side) as attributesMarkup
          // and to its right (Blueprint classes span-8 last) other data containing img and selection details as selectionMarkup.
          // selectionMarkup is top to bottom: image, then checkbox, quantity and cost input text fields right justified (CSS class prodSelectInput).
          // We also add a hiddenCost, so that the cost per item is always available for our calculation (visible to user in attributes in any event, but harder for us to get at for computation)
          def attributesMarkup(prod: IProduct, quantityAttr: Attribute, attrs: IndexedSeq[Attribute]): NodeSeq = {
            def setProdIdName( attr: Attribute) =
              if (attr == quantityAttr) prod.lcboId.toString else ""

            val tbody = {
              for (attr <- attrs)
                yield <tr>
                  <td class="prodAttrHead">
                    {attr.key}
                  </td>
                  <td class="prodAttrContent" name={setProdIdName(attr)}>
                    {attr.value}
                  </td>
                </tr>
            }
            <div class="span-8">
              <table>
                <thead>
                  <tr>
                    <th>Attribute</th>
                    <th>Value</th>
                  </tr>
                </thead>
                <tbody>
                  {tbody}
                </tbody>
              </table>
            </div>
          }

          def selectionMarkup(prod: IProduct, inventory: String) = {
            val imgNS = <img src={prod.imageThumbUrl}/>

            // create a checkBox with value being product id (key for lookups) and label's html representing name. The checkbox state is picked up when we call JS in this class
            val lcboId = prod.lcboId.toString
            val checkBoxNS =
              <label>
                <input type="checkbox" class="prodSelectInput" value={lcboId}/>
                {prod.Name}
              </label><br/>

            val quantityNS =
              <label>Item Quantity:
                <input type="text" class="prodQty prodSelectInput" onchange="prodSelection.updateQtyItem(this);" value="1"/>
              </label><br/>

            // read-only costNS, so user can see it clearly but cannot update it.
            val costNS =
              <label>Cost:
                <input type="text" class="prodCost prodSelectInput" value={prod.price} readonly="readonly"/>
              </label>

            // this info is redundant in DOM to some extent but makes it more convenient to fetch and we're not using JSON here.
            val hiddenCostNS = <input type="text" class="hiddenProdCost" value={prod.price} hidden="hidden"/>
            val hiddenInvNS = <input type="text" class="hiddenProdInv" value={inventory} hidden="hidden"/>

            val ns: NodeSeq =  <div class="span-8 last">{imgNS}<br/>{checkBoxNS}{quantityNS}{costNS}{hiddenCostNS}{hiddenInvNS}</div>
            ns
          }

          val prod = qOfProd.product
          val inventory = qOfProd.quantity.toString
          val inventoryAttribute = Attribute("Quantity:", inventory)
          val allAttributes = prod.streamAttributes :+ inventoryAttribute

          <div>{attributesMarkup(prod, inventoryAttribute, allAttributes)}{selectionMarkup(prod, inventory)}</div><hr/>

        }

        // for each element of qOfProds, build a Div as NodeSeq and concatenate them as a NodeSeq for the several divs
        val divs = qOfProds.map(getDiv).fold(NodeSeq.Empty)((a: NodeSeq, b: NodeSeq) => a ++ b)
        SetHtml("prodContainer", divs) & hideConfirmationJS & showProdDisplayJS  // JsCmd (JavaScript  (n JsCmd) can be chained as bigger JavaScript command)
      }

      def maySelect(storeId: P_KEY): JsCmd =
        Store.getStore(storeId).fold {
          S.error("We need to establish your local store first, please wait for local geo to become available")
          Noop
        }{ s =>
          // validate expected numeric input storeId then access LCBO data
          val prodQtySeq = s.recommend(theCategory.is, theRecommendCount.is) match {
            // we want to distinguish error messages to user to provide better diagnostics.
            case Full(pairs) =>
              if (pairs.isEmpty) S.error(s"Unable to choose find a product of category ${theCategory.is} that is not out of stock! Looking into it, please try again a little later")
              // we're reloading into cache to make up for that issue!
              Full(pairs) // returns prod and quantity in inventory normally, regardless of emptyness)
            case Failure(m, ex, _) => S.error(s"Unable to choose product of category ${theCategory.is} with message $m and exception error $ex"); Empty
            case Empty =>
              S.error(s"Unable to choose product of category ${theCategory.is}")
              Empty
          }
          prodQtySeq.dmap { Noop } // we gave notice of error already via JS, nothing else to do
          { pairs => // normal case
            S.error("") // work around clearCurrentNotices clear error message to make way for normal layout representing normal condition.
            prodDisplayJS( pairs.map{case ( p, q) => QuantityOfProduct(q, p)})
          }
        }

      val json = jsStore.extractOpt[String].map( parse)
      val storeId = P_KEY(json.fold(-1.toLong){ json => json.extract[Long]})
      User.currentUser.dmap { S.error("recommend", "recommend feature unavailable, Login first!"); Noop }
      { user => maySelect(storeId)} // normal processing
    }

    def consumeProducts(selection: JValue): JsCmd = {
      def transactionsConfirmationJS(user: String, confirmationMsgs: Iterable[PurchasedProductConfirmation]): JsCmd = {
        def getItem(item: PurchasedProductConfirmation): NodeSeq = {
          def purchaseConfirmationMessage(confirmation: String, formattedCost: String, quantity: Long, missedQty: Long) = {
            if (missedQty <= 0)
              s"$confirmation including the cost of today's purchase at $formattedCost for $quantity extra units"
            else
              s"$confirmation including the cost of today's purchase at $formattedCost for $quantity extra units; sorry about the unfulfilled $missedQty items out of stock"

          }

          val formattedCost = formatter format item.selectedProduct.cost
          val liContent = purchaseConfirmationMessage(item.confirmation,
            formattedCost,
            item.selectedProduct.quantity,
            item.selectedProduct.missedQty)
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
        def mayConsumeItem(p: IProduct, quantity: Long): Feedback = {
          User.currentUser.dmap { Feedback(userName="", success=false, "unable to process transaction, Login first!") }
          { user =>
            user.consume(p, quantity) match {
              case Full((userName, count)) => // the good
                Feedback(userName, success = true, s"${p.Name} $count unit(s) over the years")
              case Failure(e, ex, _) =>  // the bad
                Feedback(userName = "", success = false, s"Unable to sell you product ${p.Name} with error $e and exception '$ex'")
              case Empty =>  // the ugly
                Feedback(userName = "", success = false, s"Unable to sell you product ${p.Name}")
            }
          }
        }

        // associate primitive browser product details for selected products (SelectedProduct) with full data of same products we should have in cache as pairs
        val feedback =
          for(sp <- selectedProds;
              product <- Product.getItemByLcboId(LCBO_ID(sp.id));
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

      val jsonOpt = selection.extractOpt[String].map(parse)
      val selectedProducts = jsonOpt.map(json => {for (p <- json.children.toVector) yield p.extractOpt[SelectedProduct]}.flatten )
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
                      { j: JValue => consumeProducts(j) & setBorderJS(actionButtonsContainer, "consume")}) & // fetch in JSON with JS Call the lcbo product IDs and then deal with them
    "@cancel [onclick]" #>
      ajaxInvoke({() => cancel & setBorderJS(actionButtonsContainer, "cancel")}) &
    "select" #> ajaxSelect(RecommendCount.options, RecommendCount.default,
      { selected: String => theRecommendCount.set(toInt(selected)); Noop }) andThen // always before recommend so it takes effect so that we know how many products to recommend.
    "@recommend [onclick]" #>
      jsonCall( JE.Call("storeFinder.getTheSelectedStore"),
        { j: JValue => recommend(j) & setBorderJS(actionButtonsContainer, "recommend")})
  }
}
