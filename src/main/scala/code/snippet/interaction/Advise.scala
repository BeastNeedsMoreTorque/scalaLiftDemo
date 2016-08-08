package code.snippet.interaction

import code.model.{IProduct, Product, Store, User}
import code.model.utils.RNG
import code.snippet.SessionCache.{theCategory, theRecommendCount}
import code.model.Attribute
import net.liftweb.common.{Empty, Full}
import net.liftweb.http.S
import net.liftweb.http.js.{JE, JsCmd}
import net.liftweb.http.js.JsCmds.{SetHtml, _}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonParser._
import net.liftweb.util.Props

import scala.util.Random
import scala.xml.NodeSeq

/**
  * Created by philippederome on 2016-07-31.
  */
trait Advise extends UtilCommands {
  implicit val formats2 = net.liftweb.json.DefaultFormats
  val fetchInventoriesJS = JE.Call("inventory.fetchInventories") // let the client deal with incomplete inventories and get them himself
  val showProdDisplayJS =  JsShowId("prodDisplay") & fetchInventoriesJS

  def advise(jsStore: JValue): JsCmd =
    User.currentUser.dmap { S.error("advise", "advise feature unavailable, Login first!"); Noop } { user =>
      val cmd =
        for {s <- jsStore.extractOpt[String].map(parse) // ExtractOpt avoids MappingException and generates None on failure
             storeId <- s.extractOpt[Long]
             s <- Store.getStore(storeId)
             advice <- maySelect(s)
        } yield advice

      cmd.fold {
        S.error("We need to establish your local store first, please wait for local geo to become available"); Noop }
      { identity } // normal case
    }

  private def maySelect(s: Store): Option[JsCmd] = {
    val rng = RNG.Simple(if (Shuffler.UseRandomSeed) Random.nextInt() else Shuffler.FixedRNGSeed)
    lazy val successToProducts: Iterable[(IProduct, Long)] => Option[Iterable[(IProduct, Long)]] = {
      pairs: Iterable[(IProduct, Long)] =>
        if (pairs.isEmpty) S.error(s"Unable to find a product of category ${theCategory.is}")
        // we're reloading into cache to make up for that issue!
        Full(pairs) // returns prod and quantity in inventory normally, regardless of emptyness)
    }
    lazy val errorToProducts: Throwable => Option[Iterable[(IProduct, Long)]] = {
      t: Throwable =>
        S.error(s"Unable to choose product of category ${theCategory.is} with exception error ${t.toString()}")
        Empty
    }
    val advice = {
      val advisedProductsSeq = s.advise(rng, theCategory.is, theRecommendCount.is, Product)
      val prodQtySeq = advisedProductsSeq.fold(errorToProducts, successToProducts)
      prodQtySeq.fold { Noop } // we gave notice of error already via JS, nothing else to do
      { pairs => // normal case
        S.error("") // work around clearCurrentNotices clear error message to make way for normal layout representing normal condition.
        prodDisplayJS(pairs.map { case (p, q) => QuantityOfProduct(q, p) })
      }
    }
    Some(advice)
  }

  private def prodDisplayJS(qOfProds: Iterable[QuantityOfProduct]): JsCmd = {
    // for each element of qOfProds, build a Div as NodeSeq and concatenate them as a NodeSeq for the several divs
    val divs: NodeSeq = <div>{qOfProds.map(getDiv)}</div>
    SetHtml("prodContainer", divs) & hideConfirmationJS & showProdDisplayJS  // JsCmd (JavaScript  (n JsCmd) can be chained as bigger JavaScript command)
  }

  private def getDiv(qOfProd: QuantityOfProduct): NodeSeq = {
    val prod = qOfProd.product
    val inventory = qOfProd.quantity.toString
    val invAttribute = Attribute(key="Quantity:", value=inventory, css="prodAttrContentInventory", name=prod.lcboId.toString)
    // label it as prodAttrContentInventory for client to interpret and identify easily
    // similarly, assigning the prodId to name helps identify it in browser JS.
    val allAttributes = prod.streamAttributes :+ invAttribute

    // tag the div with misc attributes to facilitate reconciling related data within browser JS
    <div class="prodIdRoot" name={prod.lcboId.toString}>{attributesMarkup(prod, allAttributes)}{selectionMarkup(prod, inventory)}</div><hr/>
  }

  // layout description:
  // attributes in center column (Blueprint class span-8, width 8 not to side) as attributesMarkup
  // and to its right (Blueprint classes span-8 last) other data containing img and selection details as selectionMarkup.
  // selectionMarkup is top to bottom: image, then checkbox, quantity and cost input text fields right justified (CSS class prodSelectInput).
  // We also add a hiddenCost, so that the cost per item is always available for our calculation
  // (visible to user in attributes in any event, but harder for us to get at for computation)
  private def attributesMarkup(prod: IProduct, attrs: IndexedSeq[Attribute]): NodeSeq = {
    def rowMarkup(a: Attribute): NodeSeq = {
      <tr>
        <td class="prodAttrHead">{a.key}</td>
        <td class={a.css} name={a.name}>{a.value}</td>
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
        <tbody>{attrs.map(rowMarkup)}</tbody>
      </table>
    </div>
  }

  private def selectionMarkup(prod: IProduct, inventory: String) = {
    // create a checkBox with value being product id (key for lookups) and label's html representing name.
    // The checkbox state is picked up when we call JS in this class
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
    val hiddenCostNS: NodeSeq = <input type="text" class="hiddenProdCost" value={prod.price} hidden="hidden"/>
    val imgNS = <img src={prod.imageThumbUrl}/>
    (<div class="span-8 last">{imgNS}<br/>{checkBoxNS}{quantityNS}{costNS}{hiddenCostNS}</div>): NodeSeq
  }

  // message is confirmation when successful, error when not
  case class QuantityOfProduct(quantity: Long, product: IProduct)  // quantity available at the current store for a product (store is implied by context)

  object Shuffler  {
    val UseRandomSeed = Props.getBool("productInteraction.useRandomSeed", true)
    val FixedRNGSeed = Props.getInt("productInteraction.fixedRNGSeed", 0)
  }

}
