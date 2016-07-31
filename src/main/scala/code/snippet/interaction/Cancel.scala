package code.snippet.interaction

import net.liftweb.http.S
import net.liftweb.http.js.JsCmd

/**
  * Created by philippederome on 2016-07-31.
  */
trait Cancel extends UtilCommands {
  def cancel: JsCmd = {
    S.error("")
    hideProdDisplayJS & hideConfirmationJS
  }
}

