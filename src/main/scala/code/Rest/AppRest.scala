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
   * find /store/lat/43,0/lon/-80,0.json
   * find /store/lat/43,0/lon/-80,0.xml (tested with curl or JS)
   * findInRectangle /store/lat1/43,0/lon1/-80,0/lat2/44,00/lon2/-79,00.json (or .xml)
   */
  serve {
    case "store" :: "lat" :: DotDecimalString(lat) :: "lon" :: DotDecimalString(lon) :: Nil JsonGet _ =>
      for {
      // find the store, and if it's not found,
      // return a nice message for the 404
        store <- Store.find(lat, lon) ?~ s"Store Not Found near location ($lat, $lon)"
      } yield Extraction.decompose(store) // a JValue, allowing servlet to return some JSon

    case "store" :: "lat" :: DotDecimalString(lat) :: "lon" :: DotDecimalString(lon) :: Nil XmlGet _ =>
      for {
      // find the store, and if it's not found,
      // return a nice message for the 404
        store <- Store.find(lat, lon) ?~ s"Store Not Found near location ($lat, $lon)"
      } yield store: Node  // Node is what generates XML (see converters in Store)

    case "stores" :: "lat1" :: DotDecimalString(lat1) :: "lon1" :: DotDecimalString(lon1)
      :: "lat2" :: DotDecimalString(lat2) :: "lon2" :: DotDecimalString(lon2):: Nil JsonGet _ =>
      val stores = Store.findInRectangle(lat1, lon1, lat2, lon2) //?~ s"Stores Not Found within locations ($lat1, $lon1, $lat2, $lon2)"
      Extraction.decompose(stores) // a JValue, allowing servlet to return some JSon, this is a collection.

    case "stores" :: "lat1" :: DotDecimalString(lat1) :: "lon1" :: DotDecimalString(lon1)
      :: "lat2" :: DotDecimalString(lat2) :: "lon2" :: DotDecimalString(lon2):: Nil XmlGet _ =>
      val stores = Store.findInRectangle(lat1, lon1, lat2, lon2)
      <stores>{stores.map(s => {s:Node})}</stores>
  }

}
