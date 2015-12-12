package code.comet

import code.model.{DisplayStoreInstruction, Store, StoreProvider}
import net.liftweb.common._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, CometActor, CometListener, SHtml}
import net.liftweb.util.Props

/**
  * Created by philippederome on 2015-12-09.
  */
class GetStores extends CometActor with CometListener with Loggable {
  private val provider = new StoreProvider()
  // Dependency Injection (another part of the website could use a DB table!)
  private val latitude: String = Props.get("user.latitude", "")
  private val longitude: String = Props.get("user.longitude", "")
  private var store: Box[Store] = Empty

  def registerWith = StoreExchange // our publisher to whom we register interest

  override def lowPriority = {
    // use partial function for the callback to our publisher StoreExchange, we filter one type of data, cache it so that upon rendering we capture it and act accordingly
    case s: DisplayStoreInstruction => reRender()
  }

  def locate: JsCmd = {
    store = provider.findStore(latitude, longitude) match {
      case util.Success(x) =>
        x or {
          S.notice(s"no store identified at all!?");
          Empty
        }
      case util.Failure(ex) => S.error(s"Unable to locate store using user reference point ($latitude, $longitude) with error $ex"); Empty
    }
    reRender()
    Noop // async instructs ourselves to redraw (reRender, a CometActor method).
  }

  def render = {
    "#storeContent" #> store.dmap {
      ""
    } {
      _.toString
    } &
      "button [onclick]" #> SHtml.ajaxInvoke(() => {
        locate
      })
  }
}
