package code.snippet

import net.liftweb.http.js.JE.Call
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
/**
  * Created by philippederome on 2016-02-15.
  */
trait JSUtilities {
  def setBorderJS(container: String, elt: String): JsCmd = Call("toggleButton.frame", container, elt)

}
