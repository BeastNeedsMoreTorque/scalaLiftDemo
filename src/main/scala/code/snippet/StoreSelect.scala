package code.snippet

import code.snippet.SessionCache.TheStore
import net.liftweb.common.Full
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.JsCmds._
import net.liftweb.util.Helpers._

/**
  * Created by philippederome on 2015-12-05.
  */
object StoreSelect {
  def render =
    "input" #> SHtml.ajaxText(
      if (TheStore.is > 0) TheStore.is.toString else "",
      (s: String) => {
        TheStore.set(asInt(s) match {
          //validates expected numeric input store
          case Full(t) if t > 0 => S.error("store", ""); t
          case _ => S.error("store", s"enter number > 0 "); 0
        })
        Noop
      }
    )
}
