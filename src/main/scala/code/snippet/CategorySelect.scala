package code.snippet

import code.model.LiquorCategory
import code.snippet.SessionCache.theCategory
import net.liftweb.common.Empty
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds.Noop
import net.liftweb.util.Helpers._

/**
  * Created by philippederome on 2015-12-05.
  */
object CategorySelect {
  private val radioOptions = LiquorCategory.sortedSeq.map{(s: String) => RadioElements(s, <img src={LiquorCategory.toImg(s)}/>)}

  /**
    * save radio button selection as next default to avoid annoying resetting to original default and make it session persistent
    * Dev note: ajaxRadio requires as last parameter a ajaxFunc function that is [T] (String here) => JsCmd
    * ajaxRadio itself returns a ChoiceHolder[T] (ChoiceHolder[String]), i.e. a selection of liquor categories
    * which the caller can then render as HTML by applying toForm, returning a NodeSeq (with a detour to LabelStyle to lay out buttons horizontally)
    * Choosing a SHtml.ajaxRadio is what allows us to have a callback each time user changes selection (freeing us a form with a hidden input or explicit button)
    * @see http://chimera.labs.oreilly.com/books/1234000000030/ch03.html#_solution_29
    * @see http://stackoverflow.com/questions/15879991/get-checkbox-and-radio-button-value-in-lift
    */
  def render = {
    ".options" #> LabelStyle.toForm(SHtml.ajaxRadio(
    radioOptions, Empty,
    (choice: RadioElements) => {
      theCategory.set(choice.name)
      Noop
    })) andThen
      "input [hidden]" #> "true"  // to hide the classic circle of the radio button (needs to be scheduled after prior NodeSeq transformation
  }
}
