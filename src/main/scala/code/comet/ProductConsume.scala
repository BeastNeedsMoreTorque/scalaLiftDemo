package code.comet

import code.model._
import net.liftweb.common._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.http.{CometListener, CometActor, S, SHtml}
import net.liftweb.util.CssSel

/**
  * Created by philippederome on 2015-12-05.
  */
class ProductConsume extends CometActor with CometListener with CSSUtils with Loggable {
  private val provider: ProductProvider = new ProductProvider() with PersistProductIDAsDB
  // Dependency Injection (another part of the website could use a DB table!)
  private var displayInstructions: Either[ClearInstruction, Product] = Left(ClearInstruction()) // private state indicating whether to show product when one is defined (Right of Either) or clear instruction not to show old product

  def registerWith = ProductExchange // our publisher to whom we register interest

  override def lowPriority = {
    // use partial function for the callback to our publisher ProductExchange, we filter one type of data, cache it so that upon rendering we capture it and act accordingly
    case p: Either[ClearInstruction, Product] =>
      displayInstructions = p
      reRender()
  }

  def render = {
    def consume(): JsCmd = {
      def mayConsume(p: Product): JsCmd = {
        provider.consume(p) match {
          case util.Success((userName, count)) =>
            ConfirmationExchange ! s"${p.name} has now been purchased $count time(s), $userName"
            ProductExchange ! Left(ClearInstruction()) // Sends out to other snippets or comet actors (and self to disable self button) aynchronously event to clear contents of a product display as it's no longer applicable
            S.clearCurrentNotices // clears error message now that this is good, to get a clean screen.
          case util.Failure(ex) => S.error(s"Unable to sell you product ${p.name} with error '$ex'")
        }
      }
      displayInstructions match {
        case Left(c) => Noop // ignore consecutive clicks, ensuring we do not attempt to consume multiple times in a row on a string of clicks from user
        case Right(p) => mayConsume(p) // we got notified that we have a product that can be consumed and user expressed interest in consuming. So, try it as a simulation by doing a DB store.
      }
    }

    // binding using Liftweb method of CSS selectors, left-hand side for each line match a number of DOM elements, whereas RHS takes an action on selected elements.
    // Rendering below is in parallel when using & at EOL or sequential when using andThen. Sequence is independent from html element sequence.
    val addImgElem: CssSel = "button *+" #> <img src="/images/winesmall.png" alt=" "/>
    displayInstructions match {
      case Left(c) =>
        "button" #> disable & addImgElem // disables button by setting required disabled attribute. We do this when there's nothing to display/consume
      case Right(p) =>
        "button" #> SHtml.ajaxButton("Consume", () => consume()) & addImgElem // rewrite the button DOM element by specifying its label and that its callback is AJAX and function consume, which will execute JS in browser when done
    }
  }
}
