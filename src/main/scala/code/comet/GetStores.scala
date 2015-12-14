package code.comet

import code.model.{ Store, StoreProvider}
import code.snippet.SessionCache.{TheLongitude, TheLatitude}
import net.liftweb.common._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, CometActor, CometListener, SHtml}

/**
  * Created by philippederome on 2015-12-09.
  */
class GetStores extends CometActor with CometListener with Loggable {
  private val provider = new StoreProvider()
  private var msgStore = Store()
  private var storeId: Option[String] = None

  def registerWith = StoreProductExchange // our publisher to whom we register interest

  override def lowPriority = {
    // use partial function for the callback to our publisher StoreProductExchange/StoreExchange, we filter one type of data, cache it so that upon rendering we capture it and act accordingly
    case s: Store =>
      msgStore = s
      storeId = Some(s.id.toString)
      reRender()
  }

  def locate: JsCmd = {
    val store = provider.findStore(TheLatitude.is, TheLongitude.is) match {
      case util.Success(x) =>
        x or {
          S.notice(s"no store identified at all!?")
          Empty
        }
      case util.Failure(ex) => S.error(s"Unable to locate store using user reference point (${TheLatitude.is}, ${TheLongitude.is}) with error $ex"); Empty
    }
    store.dmap{Noop} { (s: Store) =>
      StoreProductExchange ! s
      S.clearCurrentNotices
    }  // async instructs ourselves to redraw (reRender, a CometActor method).
  }

  def render = {
    def content = msgStore.id match {
      case badId if badId <= 0 => ""
      case _ => msgStore.toString + " from your chosen location"
    }

    "#StoreId2 [value]" #> storeId &  // html input element has no element only attributes, so here we specify the value attribute.
    "#storeContent" #> content &
    "button [onclick]" #> SHtml.ajaxInvoke(() => {
        locate
      })
  }
}
