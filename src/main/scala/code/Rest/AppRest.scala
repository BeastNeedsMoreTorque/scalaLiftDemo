package code.Rest

import code.model.Store
import net.liftweb.http.rest.RestHelper
import net.liftweb.json.Extraction
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
   * @see http://simply.liftweb.net/index-Chapter-11.html
   * @see https://www.assembla.com/spaces/liftweb/wiki/REST_Web_Services
   * Find /store/lat/43.0/lon/-80.0.json
   * Find /store/lat/43.0/lon/-80.0.xml (tested!)
   */
  serve {
    case "store" :: "lat" :: DotDecimalString(lat) :: "lon" :: DotDecimalString(lon) :: Nil JsonGet _ =>
      for {
      // find the store, and if it's not found,
      // return a nice message for the 404
        store <- Store.find(lat, lon) ?~ s"Store Not Found near location ($lat, $lon)"
      } yield     Extraction.decompose(store)

    case "store" :: "lat" :: DotDecimalString(lat)  :: "lon" :: DotDecimalString(lon)  :: Nil XmlGet _ =>
      for {
      // find the store, and if it's not found,
      // return a nice message for the 404
        store <- Store.find(lat, lon) ?~ s"Store Not Found near location ($lat, $lon)"
      } yield store: Node  // Node is what generates XML (see converters in Store)
  }

}
