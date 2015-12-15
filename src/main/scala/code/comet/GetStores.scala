package code.comet

import code.model.{ Store, StoreProvider}
import code.snippet.SessionCache.TheUserCoordinates
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
      storeId = if (s.id <= 0) None else Some(s.id.toString)
      reRender()
  }

  def locate: JsCmd = {
    val store: Store = provider.findStore(TheUserCoordinates.is) match {
      case util.Success(x) =>
        x openOr {
          S.notice(s"no store identified at all!?")
          Store()
        }
      case util.Failure(ex) => S.error(s"Unable to locate store using user reference point ${TheUserCoordinates.is} with error $ex"); Store()
    }
    StoreProductExchange ! store // async instructs ourselves to redraw (reRender, a CometActor method).
    if (store.id <=0) Noop
    else S.clearCurrentNotices
  }

  def render = {
    val content =
      if (msgStore.id <= 0)  None
      else Some(msgStore.toString + " from your chosen location")

    "#StoreId2 [value]" #> storeId &  // html input element has no element only attributes, so here we specify the value attribute.
    "#storeContent" #> content &
    "button [onclick]" #> SHtml.ajaxInvoke(() => {
        locate
      })
  }
}
