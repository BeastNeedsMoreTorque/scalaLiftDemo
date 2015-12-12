package code.snippet

import code.comet.{ConfirmationExchange, ProductExchange}
import code.model.ClearInstruction
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._

/**
  * Stateless snippet that allows user to come back to previous page by cancelling product recommendation and refusing its consumption.
  */
object Cancel {
  def button =
    "button [onclick]" #> SHtml.ajaxInvoke(() => {
      // AJAX as always, no downside to use it.
      ConfirmationExchange ! ""
      ProductExchange ! Left(ClearInstruction()) // Sends out to other snippets asynchronously event to clear contents of a product display as it's no longer applicable
    }) &
      "button *+" #> <img src="/images/cancel.png" alt=" "/> // since above completely wiped out the XML (HTML5) node, we just lost the nice image with button, so add it back by adding a DOM element with img underneath the button.
}
