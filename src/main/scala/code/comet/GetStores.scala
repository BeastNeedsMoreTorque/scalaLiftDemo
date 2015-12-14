package code.comet

import code.model.{ Store, StoreProvider}
import net.liftweb.common._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.{S, CometActor, CometListener, SHtml}
import net.liftweb.util.Props
import scala.xml.Text
/**
  * Created by philippederome on 2015-12-09.
  */
class GetStores extends CometActor with CometListener with Loggable {
  private val provider = new StoreProvider()
  // Dependency Injection (another part of the website could use a DB table!)
  private val latitude: String = Props.get("user.latitude", "")
  private val longitude: String = Props.get("user.longitude", "")
  private var msgStore = Store()

  def registerWith = StoreProductExchange // our publisher to whom we register interest

  override def lowPriority = {
    // use partial function for the callback to our publisher StoreProductExchange/StoreExchange, we filter one type of data, cache it so that upon rendering we capture it and act accordingly
    case s: Store => msgStore = s; reRender()
  }

  def locate: JsCmd = {
    val store = provider.findStore(latitude, longitude) match {
      case util.Success(x) =>
        x or {
          S.notice(s"no store identified at all!?");
          Empty
        }
      case util.Failure(ex) => S.error(s"Unable to locate store using user reference point ($latitude, $longitude) with error $ex"); Empty
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
    "#storeContent" #> content &
    "button [onclick]" #> SHtml.ajaxInvoke(() => {
        locate
      })
  }
}
