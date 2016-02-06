package code.Rest

import code.model.Store
import net.liftweb.http.LiftRules
import net.liftweb.http.rest.RestHelper
import net.liftweb.json._
import scala.xml.Node

/**
  * Created by philippederome on 2015-12-19.
  *
  * @see http://simply.liftweb.net/index-5.3.html#prev
  */
object AppRest extends RestHelper {
  def init(): Unit =
    LiftRules.dispatch.append(AppRest)

  /*
   * Serve the URL, but have a helpful error message when you
   * return a 404 if the item is not found
   * @see http://simply.liftweb.net/index-5.3.html#prev
   * @see http://simply.liftweb.net/index-Chapter-11.html
   * @see https://www.assembla.com/spaces/liftweb/wiki/REST_Web_Services
   * find /store/lat/43,0/lng/-80,0.json
   * find /store/lat/43,0/lng/-80,0.xml (tested with curl or JS)
   * findInRectangle /store/swlat/43,0/swlng/-80,0/nelat/44,00/nelng/-79,00.json (or .xml)
   */
  serve {
    case "stores" :: "lat" :: DotDecimalString(lat) :: "lng" :: DotDecimalString(lng) :: Nil JsonGet _ =>
      for {
      // find the store, and if it's not found,
      // return a nice message for the 404
        store <- Store.find(lat, lng) ?~ s"Store Not Found near location ($lat, $lng)"
      } yield Extraction.decompose(store) // a JValue, allowing servlet to return some JSon

    case "stores" :: "lat" :: DotDecimalString(lat) :: "lng" :: DotDecimalString(lng) :: Nil XmlGet _ =>
      for {
      // find the store, and if it's not found,
      // return a nice message for the 404
        store <- Store.find(lat, lng) ?~ s"Store Not Found near location ($lat, $lng)"
      } yield store: Node  // Node is what generates XML (see converters in Store)

    case "stores" :: "swlat" :: DotDecimalString(swlat) :: "swlng" :: DotDecimalString(swlng)
      :: "nelat" :: DotDecimalString(nelat) :: "nelng" :: DotDecimalString(nelng):: Nil JsonGet _ =>
      val stores = Store.findInRectangle(swlat, swlng, nelat, nelng) //?~ s"Stores Not Found within locations (...)"
      Extraction.decompose(stores) // a JValue, allowing servlet to return some JSon, this is a collection.

    case "stores" :: "swlat" :: DotDecimalString(swlat) :: "swlng" :: DotDecimalString(swlng)
      :: "nelat" :: DotDecimalString(nelat) :: "nelng" :: DotDecimalString(nelng):: Nil XmlGet _ =>
      val stores = Store.findInRectangle(swlat, swlng, nelat, nelng) //?~ s"Stores Not Found within locations (...)"
      <stores>{stores.map(s => {s:Node})}</stores>
  }

}
