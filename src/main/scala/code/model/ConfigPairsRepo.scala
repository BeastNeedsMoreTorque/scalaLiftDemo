package code.model

import net.liftweb.json._
import net.liftweb.util.Props
import scala.collection.Seq

/**
  * Created by philippederome on 2016-04-08.
  * A module offering several possible different implementations of ConfigPairsRepo
  */
object ConfigPairsRepo {
  implicit val formats = net.liftweb.json.DefaultFormats
  val emptyString: String = ""
  val emptyPairs = Seq.empty[(String, String)]
  /**
    * represents a map of String to Map(key:String, value:String) that is configurable at run time.
    * @param masterKey a key to some configuration holding a map of values
    * @return a map of strings to strings bound to the masterKey
    */
  def getSeq(masterKey: String): Seq[(String, String)] = {
    val json = parseOpt(Props.get(masterKey, emptyString)) // assumed to be of form {"key1":"value1",... "keyn":"valuen"}, which is JSON
    // contains optionally children having JValue, which are really JField(name:String, value:JValue that is effectively String)
    // should now be an Option on JObject(List(JFields))
    json.map {
      case (JObject(xs)) => xs
      case _ => Seq.empty[JField]
    }.map(_.map { case(JField(k, JString(v))) => (k, v) } )
      .map(identity).getOrElse(emptyPairs)
  }

}
