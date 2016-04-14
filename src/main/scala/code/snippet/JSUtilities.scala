package code.snippet

import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
/**
  * Created by philippederome on 2016-02-15.
  */
object JSUtilities {
  /**
    *
    * @param DomRootContainer container from where JS query will look for
    * @param DomElementName pattern in JS query to match is with name as DomElementName
    * @return a JavaScript command that Lift will execute, effect is to turn on thicker select for DomElementName and reduce what used to be thicker (selected)
    *         for other elements within the same group of image radio buttons (as per selection of DomRootContainer that specifies the group).
    *         First parameter of Call is the name of the JS function
    */
  def setBorderJS(DomRootContainer: String, DomElementName: String): JsCmd =
    Call("toggleButton.frame", DomRootContainer, DomElementName)
}
