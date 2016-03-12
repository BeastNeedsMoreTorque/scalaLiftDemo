package code.Rest

import code.model.Store
import net.liftweb.http.LiftRules
import net.liftweb.http.rest.RestHelper
import net.liftweb.json._
import scala.xml.Node

/**
  * Created by philippederome on 2015-12-19.
  *
  * we use attach point stores JSON way (XML available for completeness)
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
   */
  serve {
    case "stores" :: Nil JsonGet _ =>
      val stores = Store.findAll().map{_.asJValue}
      Extraction.decompose(stores) // a JValue, allowing servlet to return some JSon, this is a collection.

    case "stores" :: Nil XmlGet _ =>
      val stores = Store.findAll()
      <stores>{stores.map(s => {s:Node})}</stores>
  }

}
