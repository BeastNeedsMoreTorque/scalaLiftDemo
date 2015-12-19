package code.comet

import code.model.Store
import code.snippet.SessionCache.TheStore
import net.liftweb.common.{Full, Loggable}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{CometActor, CometListener, S, SHtml}
import net.liftweb.util.Helpers._

import scala.xml.Text

/**
  * Created by philippederome on 2015-12-13.
  */
class StoreSelect extends CometActor with CometListener with Loggable{

  def registerWith = StoreProductExchange

  import scala.language.postfixOps
  override def lifespan = Full(120 seconds)

  // listen for data published by other snippets notifying us of new data to act upon.
  override def lowPriority = {
    // use partial function
    case s: Store => // is this Model-View-Controller in one spot? Ha ha.
      TheStore.set(s.id)
      SetHtml("storeId2", Text(s.id.toString))
      reRender()
  }

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
