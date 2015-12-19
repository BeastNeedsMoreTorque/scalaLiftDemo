package code.Rest

import code.model.{GeoCoordinates, Store}
import net.liftweb.http._


/**
  * Created by philippederome on 2015-12-16.
  */

object AppRest  {

  /*
   * Find /store/lat/43.0/lon/-80.0.json (TEST!)
   * Find /store/lat/43.0/lon/-80.0.xml
   */
  lazy val findClosestStore: LiftRules.DispatchPF = {
    case Req("store" :: "lat" :: lat :: "lon" :: lon :: Nil, //  path
    suffix, // suffix
    GetRequest) =>
      () => Store.find(GeoCoordinates(lat, lon)).map(toResponse(suffix, _))

  }

  /*
   * Given a suffix and a store, make a LiftResponse
   */
  private def toResponse(suffix: String, s: Store) = {
    suffix match {
      case "xml" => XmlResponse(s)
      case _ => JsonResponse(s)
    }
  }

}
