package code.snippet.interaction

import code.snippet.JSUtilities._
import code.snippet.SessionCache._
import net.liftweb.common._
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.{JE, JsCmd}
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

/**
  * This is a Lift Snippet: it plays the equivalent of a controller and a view with render being responsible to render to client (a bit like a PLAY! Action)
  * Lift embraces statefulness while Play eschews it. So we have state in HTTP session (not in cookie).
  *
  * Lift uses JS in request chain at start of event (see consume making use of jsonCall), then it does its thing on server side, much like a Controller.
  * Then it executes a JS command, which effects the browser; that JS command can be a chain of JS commands and we use that heavily.
  * This means much JS control actually takes place here.
  * Snippet does not use REST. REST API is used elsewhere in app and could be used more.
  * Comets is available in Lift and apparently compares favourably with Akka technology. I don't use it.
  * HTTP request chain is asynchronous from browser perspective (no page reload); however the render method is synchronous.
  * For full asynchronicity from web to browser, comet is the available technology (so this is not like standard PLAY Action).
  *
  * Snippets are doing transformation of XML nodes in the document and is DEFINITELY not HTML page centric, meaning we target part of a page.
  * Snippets can embed each other on a html page to reduce markup duplication.
  * The snippet transformation of rewriting a XML node is done at the end of the render function matching elements in CSS style and using some LIFT DSL ( #> )
  * to mean "rewrite".
  * The various elements that are rewritten occur in parallel unless parallelism is turned off with "andThen" as per other Scala idioms.
  * The parallel chaining is done with &.
  *
  * There's no template engine in Lift, the html pages are pure html and contain no Scala code.
  * The design view seems to be that Scala developers can build everything in the stack and they don't want to get into JS too often;
  * also the no-template approach is apparently meant to prevent web designers to check in changes to markup
  * that break business logic (in Scala code) by accident by not letting them near the Scala code.
  *
  * This snippet contains state only via HTTP session (A Liftweb snippet is a web page fragment that has autonomous existence
  * equivalent to a page with lift framework
  * being responsible to render all snippets on a page and to handle events between snippets and corresponding html div elements)
  * Much html and Javascript is generated here thanks to the capabilities of liftweb.
  *
  * Created by philippederome on 15-10-26.
  */
class ProductInteraction extends Cancel with Consume with Advise with Loggable {
  /**
    * JS instruction to hide the product display
    */
  val hideProdDisplayJS: JsCmd =  JsHideId("prodDisplay")
  /**
    * JS instruction to show the confirmation div element
    */
  val showConfirmationJS: JsCmd =  JsShowId("confirmationDiv")
  /**
    * JS instruction to hide the confirmation div element
    */
  val hideConfirmationJS: JsCmd =  JsHideId("confirmationDiv")

  /**
    * A transformation on NodeSeq called by Lift, with NodeSeq representing a sequence of XML nodes (well formed HTML).
    * This entry point is identified by using reflection specifying class ProductInteraction in html code to come in here.
    *
    * Here we do 4 rewrites, one for the ? (advise) button, the x (cancel) button, the glass (consume) button
    * and the advise count drop down select menu. Each button activation is represented by a corresponding trait.
    * The select menu of # items is always evaluated serially before the advise button callback is activated
    * so that we know properly the right number of products to advise.
    *
    * @return transformation on NodeSeq
    */
  def render: NodeSeq => NodeSeq = {
    def activateBt(name: String) = setBorderJS("prodInteractionContainer", name) // prodInteractionContainer is html markup identifier

    theAdviseCount.set(toInt(AdviseCount.default))

    // call to activateBt after button activation simply highlights that button was pressed, so we do it on each onclick action below.
    "@consume [onclick]" #>
      jsonCall( JE.Call("prodSelection.currentProds"),
                      ( j => consume(j) & activateBt("consume"))) &
      // fetch in JSON with JS Call the lcbo product IDs and then deal with them
    "@cancel [onclick]" #>
      ajaxInvoke({() => cancel & activateBt("cancel")}) &
    "select" #> ajaxSelect(AdviseCount.options, AdviseCount.default,
      { selected: String => theAdviseCount.set(toInt(selected)); Noop }) andThen
      // always before recommend so it takes effect so that we know how many products to recommend.
    "@advise [onclick]" #>
      jsonCall( JE.Call("storeFinder.getTheSelectedStore"),
        ( j => advise(j) & activateBt("advise")))
  }

  /**
    * Data store for a count of products select menu, could easily be generalized to be configured from properties
    */
  object AdviseCount {
    val values = List("1","5","10","20","50")
    val default = Full(values.head)
    val options = values zip values
  }
}
