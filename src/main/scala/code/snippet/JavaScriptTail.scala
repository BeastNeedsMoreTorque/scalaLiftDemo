package code.snippet

import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.jsExpToJsCmd

import net.liftweb.http.js.JsCmds.{Script, OnLoad}
import net.liftweb.util.Helpers._

/**
  * Created by philippederome on 2015-12-08.
  * Generates following at bottom of body:
  * jQuery(document).ready(function() {lcboViewer.getLocation();});
  */
class JavaScriptTail {
  def render =
    "*" #> <lift:tail>
      {Script(OnLoad(JsRaw("lcboViewer.start()")))}
    </lift:tail>
}
