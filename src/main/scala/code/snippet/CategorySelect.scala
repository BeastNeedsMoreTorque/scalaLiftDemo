package code.snippet

import code.model.LiquorCategory
import code.snippet.SessionCache.theCategory
import net.liftweb.common.Full
import net.liftweb.http.SHtml
import net.liftweb.util.Helpers._
import JSUtilities.setBorderJS
import scala.xml.NodeSeq
/**
  * Created by philippederome on 2015-12-05.
  */
class CategorySelect {
  private val liquorCategory = new LiquorCategory
  // hardcoded effectively as Lift does not provide parameters to classes it instantiates

  private val categoryImg = liquorCategory.toImg(SessionCache.defaultCategory)
  private val defaultOption = RadioElements.selectOption(SessionCache.defaultCategory, categoryImg)
  // selected with style that frames it

  private val radioOptions: Seq[RadioElements] =
    RadioElements.radioOptions( liquorCategory.categories, SessionCache.defaultCategory, liquorCategory.toImg)

  /**
    * save radio button selection as next default to avoid annoying resetting to original default and make it session persistent
    * Dev note: ajaxRadio requires as last parameter a ajaxFunc function that is [T] (String here) => JsCmd
    * ajaxRadio itself returns a ChoiceHolder[T] (ChoiceHolder[String]), i.e. a selection of liquor categories
    * which the caller can then render as HTML by applying toForm, returning a NodeSeq (with a detour to LabelStyle to lay out buttons horizontally)
    * Choosing a SHtml.ajaxRadio is what allows us to have a callback each time user changes selection
    * (freeing us a form with a hidden input or explicit button)
    * @see http://chimera.labs.oreilly.com/books/1234000000030/ch03.html#_solution_29
    * @see http://stackoverflow.com/questions/15879991/get-checkbox-and-radio-button-value-in-lift
    */
  def render: NodeSeq => NodeSeq = {
    theCategory.set(SessionCache.defaultCategory)  // on refresh, we set category to defaultOption and hence we should change Session State too.
    ".options" #> LabelStyle.toForm(SHtml.ajaxRadio(
      radioOptions, Full(defaultOption),
      (choice: RadioElements) => {
        theCategory.set(choice.name)
        setBorderJS("prodCategoryContainer", choice.name)
      }))
  }
}
