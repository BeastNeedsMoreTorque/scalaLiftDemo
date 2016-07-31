package code.snippet.interaction

import net.liftweb.http.js.JsCmd

/**
  * Created by philippederome on 2016-07-31.
  */
trait UtilCommands {
  val hideProdDisplayJS: JsCmd
  val hideConfirmationJS: JsCmd
  val showConfirmationJS: JsCmd
}
