package code.snippet.interaction

import code.model.{IProduct, Product, User}
import code.model.GlobalLCBO_IDs.LCBO_ID
import net.liftweb.http.S
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.{SetHtml, _}
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonParser._

import scala.xml.{NodeSeq, Text}
import java.text.NumberFormat

/**
  * Created by philippederome on 2016-07-31.
  */
case class SelectedProduct(id: Long, quantity: Long, cost: Double, missedQty: Long)

trait Consume extends UtilCommands {
  val formatter = NumberFormat.getCurrencyInstance()
  implicit val formats = net.liftweb.json.DefaultFormats

  def consumeProducts(selection: JValue): JsCmd = {
    // Validate and report errors at high level of functionality as much as possible
    // and then get down to business with helper mayConsume.
    User.currentUser.dmap { S.error("consumeProducts", "unable to process transaction, Login first!"); Noop } { user =>
      val selectedProducts =
        // ExtractOpt avoids MappingException and generates None on failure
        for (json <- selection.extractOpt[String].map(parse).toIndexedSeq;
             item <- json.children.toVector;
             prod <- item.extractOpt[SelectedProduct])
          yield prod

      if (selectedProducts.nonEmpty) mayConsume(user, selectedProducts)
      else {
        S.warning("Select some recommended products before attempting to consume")
        Noop
      }
    }
  }

  private def mayConsume(user: User, selectedProds: IndexedSeq[SelectedProduct]): JsCmd = {
    // associate primitive browser product details for selected products (SelectedProduct)
    // with full data of same products we should have in cache as pairs
    val feedback = for(sp <- selectedProds;
        p <- Product.getItemByLcboId(LCBO_ID(sp.id));
        f = mayConsumeItem(user, p, sp.quantity)) yield SelectedProductFeedback(sp, f)

    val goodAndBackFeedback = feedback.groupBy(_.feedback.success)
    // splits into errors (false success) and normal confirmations (true success)
    // as a map keyed by Booleans possibly of size 0, 1 (not 2)

    // Notify users of serious errors for each product (false group), which might be a DB issue.
    goodAndBackFeedback.getOrElse(false, Nil).map( _.feedback.message).foreach(S.error)
    // open the Option for false lookup in map, which gives us list of erroneous feedback, then pump the message into S.error

    val goodFeedback = goodAndBackFeedback.getOrElse(true, Nil) // select those for which we have success and positive message
    if (goodFeedback.isEmpty) {
      if (feedback.isEmpty) {
        S.error("None of the selected products exist in database, wait a minute or two")
        // that means web server should warm up quite a few at first.
        // Not better than Twitter Fail Whale...
      } else {
        Noop // we already provided bad feedback, here satisfy function signature (and semantics) to return a JsCmd.
      }
    }
    else {
      if (goodFeedback.size == feedback.size) S.error("") // no error, erase old errors no longer applicable.
      val confirmationMessages = goodFeedback.map{ x => PurchasedProductConfirmation(x.selectedProduct, x.feedback.message) }
      // get some particulars about cost and quantity in addition to message
      transactionsConfirmationJS(user.firstName.get, confirmationMessages) &
        hideProdDisplayJS & showConfirmationJS   // meant to simulate consumption of products
    }
  }

  private def mayConsumeItem(user: User, p: IProduct, quantity: Long): Feedback = {
    val successToFeedback: Long => Feedback =
      count => Feedback(user.firstName.get, success = true, s"${p.Name} $count unit(s) over the years")

    val errorToFeedback: Throwable => Feedback =
      t =>
        Feedback(user.firstName.get,
          success = false,
          s"Unable to sell you product ${p.Name} with exception '${t.toString()}'")

    user.consume(p, quantity).fold[Feedback](errorToFeedback, successToFeedback)
  }

  private def transactionsConfirmationJS(user: String,
                                         confirmationMsgs: Iterable[PurchasedProductConfirmation]): JsCmd = {
    val totalCost = confirmationMsgs.map{ _.selectedProduct.cost}.sum
    val formattedTotalCost = formatter.format(totalCost)
    val listItems = confirmationMsgs.map(getItem).fold(NodeSeq.Empty)( _ ++ _ )

    SetHtml("transactionsConfirmationUser", Text(user)) &
      SetHtml("purchaseAmount", Text(formattedTotalCost)) &
      SetHtml("transactionsConfirmation", listItems) &
      showConfirmationJS
  }

  private def getItem(item: PurchasedProductConfirmation): NodeSeq = {
    val formattedCost = formatter.format(item.selectedProduct.cost)
    def purchaseConfirmationMessage(confirmation: String, formattedCost: String, quantity: Long, missedQty: Long) = {
      if (missedQty <= 0)
        s"$confirmation including the cost of today's purchase at $formattedCost for $quantity extra units"
      else
        s"$confirmation including the cost of today's purchase at $formattedCost for $quantity extra units;" +
          s"sorry about the unfulfilled $missedQty items out of stock"
    }

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
