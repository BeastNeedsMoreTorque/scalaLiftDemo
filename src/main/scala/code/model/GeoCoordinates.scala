package code.model

import code.snippet.SessionCache.TheUserCoordinates

/**
  * Created by philippederome on 2015-12-14.
  */
case class GeoCoordinates(lat: String, lon: String) {
// immutable so that both pieces of data need to be specified at once ( a bit of a classic on immutability admittedly).
  override def toString = f"(${TheUserCoordinates.is.lat.toDouble}%1.6f, ${TheUserCoordinates.is.lon.toDouble}%1.6f )"
}
