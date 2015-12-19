package code.model

import code.snippet.SessionCache.{TheStore, TheUserCoordinates}
import net.liftweb.common.{Full, Empty, Box, Loggable}
import net.liftweb.json._

import scala.xml.Node

/**
  * Created by philippederome on 15-11-01.
  * This is captured from JSON parsing.
  */
case class Store(id: Int = 0,
                 is_dead: Boolean = true,
                 name: String = "",
                 distance_in_meters: Int = 0) extends Loggable {
  // intentional aliasing allowing more standard naming convention.
  val isDead = is_dead

  // intentional change of scale from metres to kilometres, using String representation instead of integer and keeping 3 decimals (0.335 ml for beer)
  def distanceInKMs: String = {
    val v = distance_in_meters.toInt / 1000.0
    f"$v%1.1f KM(s)"
  }

  override def toString = s"$id, $name; distance is:$distanceInKMs"
}

object Store extends Loggable {
  private implicit val formats = net.liftweb.json.DefaultFormats
  private val provider = new StoreProvider()

  /**
    * Convert a store to XML
    */
  implicit def toXml(st: Store): Node =
    <item>{Xml.toXml(st)}</item>


  /**
    * Convert the store to JSON format.  This is
    * implicit and in the companion object, so
    * a Store can be returned easily from a JSON call
    */
  implicit def toJson(st: Store): JValue =
    Extraction.decompose(st)


  /**
    * Find the closest store by coordinates
    */
  def find(geo: GeoCoordinates): Box[Store] = synchronized {
    TheUserCoordinates.set(geo)
    provider.findStore(geo) match {
      case util.Success(Full(x)) =>
        TheStore.set(x)
        Full(x)
      case _ => Empty
    }
  }

}