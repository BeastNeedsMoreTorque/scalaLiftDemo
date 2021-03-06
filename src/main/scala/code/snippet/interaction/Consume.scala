package code.snippet.interaction

import code.model.{IProduct, Product, Store, User}
import code.model.GlobalLCBO_IDs._
import net.liftweb.http.S
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{SetHtml, _}
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonParser._

import scala.xml.{NodeSeq, Text}
import java.text.NumberFormat

/**
  * Created by philippederome on 2016-07-31.
  */
case class SelectedProduct(id: Long, quantity: Long, cost: Double, missedQty: Long)

trait Consume extends UtilCommands {
  val formatter = NumberFormat.getCurrencyInstance()

  def consume(selection: JValue): JsCmd = {
    implicit val formats = net.liftweb.json.DefaultFormats

    val data = selection.extractOpt[String].fold[JValue](JNothing)(parse) // {"store":storeId, "items" : <list>}
    val store = for {
      id <- (data \ "store").extractOpt[Long]
      s <- Store.getStore(id.PKeyID)
    } yield s
    val selectedProducts = for {
      item <- (data \ "items").children.toIndexedSeq
      prod <- item.extractOpt[SelectedProduct]
    } yield prod
    // Validate and report errors at high level of functionality as much as possible
    // and then get down to business with helper mayConsume.
    (User.currentUser.toOption, selectedProducts, store) match {
      case (None, _, _) => S.error("consumeProducts", "unable to process transaction, Login first!"); Noop
      case (Some(user), products, _) if products.isEmpty =>
        S.warning("Select some recommended products before attempting to consume")
        Noop
      case (Some(user), _, None) => S.warning("Store must be defined to consume recommended products"); Noop
      case (Some(user), products, Some(stored)) => mayConsume(stored, user, products)
    }
  }

  private def mayConsume(store: Store, user: User, selectedProds: IndexedSeq[SelectedProduct]): JsCmd = {
    // associate primitive browser product details for selected products (SelectedProduct)
    // with full data of same products we should have in cache as pairs
    val selectedProductFeedback = for {
      sp <- selectedProds
      p <- Product.getItemByLcboKey(sp.id.LcboKeyID)
    } yield SelectedProductFeedback(sp, mayConsumeItem(store, user, p, sp.quantity))

    val goodAndBackFeedback = selectedProductFeedback.groupBy(_.feedback.success)
    // splits into errors (false success) and normal confirmations (true success)
    // as a map keyed by Booleans possibly of size 0, 1 (not 2)

    // Notify users of serious errors for each product (false group), which might be a DB issue.
    goodAndBackFeedback.getOrElse(false, Nil).map( _.feedback.message).foreach(S.error)
    // open the Option for false lookup in map, which gives us list of erroneous feedback, then pump the message into S.error

    val goodFeedback = goodAndBackFeedback.getOrElse(true, Nil) // select those for which we have success and positive message
    (goodFeedback, selectedProductFeedback) match {
      case (Nil, fs) if fs.isEmpty =>
        S.error("None of the selected products exist in database, wait a minute or two")
        // that means web server should warm up quite a few at first. Not better than Twitter Fail Whale...
      case (Nil, _) => Noop // we already provided bad feedback, here satisfy function signature (and semantics) to return a JsCmd.
      case (good, f) =>
        if (good.size == f.size) S.error("") // no error, erase old errors no longer applicable.
        val confirmationMessages = good.map { x => PurchasedProductConfirmation(x.selectedProduct, x.feedback.message) }
        // get some particulars about cost and quantity in addition to message
        transactionsConfirmationJS(user.firstName.get, confirmationMessages) &
          hideProdDisplayJS & showConfirmationJS   // meant to simulate consumption of products
    }
  }

  private def mayConsumeItem(store: Store, user: User, p: IProduct, quantity: Long): Feedback = {
    val successToFeedback: Long => Feedback =
      count => Feedback(userName = user.firstName.get, success = true,
                        message = s"${p.Name} $count unit(s) over the years")

    val errorToFeedback: Throwable => Feedback =
      t => Feedback(userName = user.firstName.get, success = false,
                    message = s"Unable to sell you product ${p.Name} with exception '${t.toString}'")

    store.consume(user, p, quantity).fold[Feedback](errorToFeedback, successToFeedback)
  }

  private def transactionsConfirmationJS(user: String,
                                         confirmationMsgs: Iterable[PurchasedProductConfirmation]): JsCmd = {
    val totalCost = confirmationMsgs.map{ _.selectedProduct.cost}.sum
    val formattedTotalCost = formatter.format(totalCost)
    val listItems = <div>{confirmationMsgs.map(getItem)}</div>

    SetHtml("transactionsConfirmationUser", Text(user)) &
      SetHtml("purchaseAmount", Text(formattedTotalCost)) &
      SetHtml("transactionsConfirmation", listItems) &
      showConfirmationJS
  }

  private def getItem(item: PurchasedProductConfirmation): NodeSeq = {
    val formattedCost = formatter.format(item.selectedProduct.cost)
    def purchaseConfirmationMessage(confirmation: String, formattedCost: String, quantity: Long, missedQty: Long) =
      if (missedQty <= 0)
        s"$confirmation including the cost of today's purchase at $formattedCost for $quantity extra units"
      else
        s"""$confirmation including the cost of today's purchase at $formattedCost for $quantity extra units;
         |sorry about the unfulfilled $missedQty items out of stock""".stripMargin

    val liContent = purchaseConfirmationMessage(item.confirmation,
      formattedCost,
      item.selectedProduct.quantity,
      item.selectedProduct.missedQty)
    <li>{liContent}</li>
  }

  case class Feedback(userName: String, success: Boolean, message: String) // outcome of userName's selection of a product,
  // to capture user input via JS and JSON (stick to Long to simplify interface with JS)
  case class SelectedProductFeedback(selectedProduct: SelectedProduct, feedback: Feedback)
  case class PurchasedProductConfirmation(selectedProduct: SelectedProduct, confirmation: String)
}
