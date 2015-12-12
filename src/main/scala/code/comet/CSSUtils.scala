package code.comet

import net.liftweb.util.Helpers._

/**
  * Created by philippederome on 2015-12-05.
  */
trait CSSUtils {
  /**
    *
    * set CSS disabled attribute to disabled (i.e. DOM element will have disabled="disabled" in it). enable reverts the effect of disable.
    */
  def disable = "* [disabled]" #> "disabled" // credit to Liftweb community (clever)...

  // Not used, but if ever we need to maintain 2+ buttons in a form and disable some buttons, this is a gem.
  def enable = "disabled=disabled [disabled]" #> (None: Option[String]) // credit to Liftweb community (remarkably clever and concise, particularly selector on LHS of #>)...

}
