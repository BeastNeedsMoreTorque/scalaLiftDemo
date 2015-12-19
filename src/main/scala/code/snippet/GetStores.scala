package code.snippet

import code.model.{GeoCoordinates, Store, StoreProvider}
import code.snippet.SessionCache.{TheStore, TheUserCoordinates}
import net.liftweb.util.Helpers._
import net.liftweb.common.{Loggable, Full, Empty, Box}


/**
  * Created by philippederome on 2015-12-09.
  */
object GetStores extends Loggable {
  private val provider = new StoreProvider()

  def locate(geo: GeoCoordinates): Box[Store] = {
    TheUserCoordinates.set(geo)

    logger.info(s"GetStores.locate $TheUserCoordinates ${TheStore.is}")
    provider.findStore(geo) match {
      case util.Success(Full(x)) =>
        TheStore.set(x.id)
        Full(x)
      case _ => Empty
    }
  }

  def render = {
    "#storeId [value]" #> TheStore.is.toString
  }
}
