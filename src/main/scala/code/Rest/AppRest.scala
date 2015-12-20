package code.Rest

import code.model.Store
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.JsonAST.JValue
import scala.xml.Node

/**
  * Created by philippederome on 2015-12-19.
  * @see http://simply.liftweb.net/index-5.3.html#prev
  */
object AppRest extends RestHelper {

  /*
   * Serve the URL, but have a helpful error message when you
   * return a 404 if the item is not found
   * @see http://simply.liftweb.net/index-5.3.html#prev
   * Find /store/lat/43.0/lon/-80.0.json
   * Find /store/lat/43.0/lon/-80.0.xml (tested!)
   */
  serve {
    case "store" :: "lat" :: lat :: "lon" :: lon :: Nil JsonGet _ =>
      for {
      // find the store, and if it's not found,
      // return a nice message for the 404
        store <- Store.find(lat, lon) ?~ "Store Not Found"
      } yield store: JValue

    case "store" :: "lat" :: lat :: "lon" :: lon :: Nil XmlGet _ =>
      for {
      // find the store, and if it's not found,
      // return a nice message for the 404
        store <- Store.find(lat, lon) ?~ "Store Not Found"
      } yield store: Node

  }
}
