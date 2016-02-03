package code.snippet

import code.model.LiquorCategory
import code.snippet.SessionCache.theCategory
import net.liftweb.common.Full
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import net.liftweb.util.Helpers._

/**
  * Created by philippederome on 2015-12-05.
  */
object CategorySelect {
  private val defaultOption = RadioElements.selectOption(SessionCache.defaultCategory, LiquorCategory.toImg(SessionCache.defaultCategory) )  // selected with style that frames it
  private val radioOptions: Seq[RadioElements] = RadioElements.radioOptions( LiquorCategory.sortedSeq, SessionCache.defaultCategory, LiquorCategory.toImg)

  def setBorderJS(elt: String): JsCmd = Call("toggleImage.frameRadioImage", "prodCategoryContainer", {elt})

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
      radioOptions, Full(defaultOption),
      (choice: RadioElements) => {
        theCategory.set(choice.name)
        setBorderJS(choice.name)
      }))
  }
}
