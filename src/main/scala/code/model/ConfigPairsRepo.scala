package code.model

import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.{JString, JField, JObject, parseOpt}
import net.liftweb.util.Props
import scala.collection.Seq

/**
  * Created by philippederome on 2016-04-08.
  * Interface to get a sequence of key values that are related by a masterKey.
  */
trait ConfigPairsRepo {
  /**
    * represents a map of String to Map(key:String, value:String) that is configurable at run time.
    * @param masterKey a key to some configuration holding a map of values
    * @return a map of strings to strings bound to the masterKey
    */
  def getSeq(masterKey: String): Seq[(String, String)]
}

/**
  * Created by philippederome on 2016-04-08.
  * A module offering several possible different implementations of ConfigPairsRepo
  */
object ConfigPairsRepo {
  // provide as many implementations of ConfigPairsRepo as required.

  /**
    * a particular ConfigPairsRepo implementation based on Props
    * @return a PropsSeqReader object
    */
  implicit def configPairsRepoPropsImpl: ConfigPairsRepo = new PropsSeqReader

  /**
    * client may not care about which one, and since we have not coded a different one, say a database one, just use the same.
    */
  implicit val defaultInstance = new PropsSeqReader

  /**
    * Props based class that uses JSON parsing from a props config to implement ConfigPairsRepo getSeq
    */
  class PropsSeqReader extends ConfigPairsRepo {
    implicit val formats = net.liftweb.json.DefaultFormats
    val emptyString: String = ""
    /**
      * @param masterKey a key to some configuration holding a map of values
      * @return a map of strings to strings bound to the masterKey
      */
    override def getSeq(masterKey: String): Seq[(String, String)] = {
      val values = Props.get(masterKey, emptyString) // assumed to be of form {"key1":"value1",... "keyn":"valuen"}, which is JSON
      // contains optionally children having JValue, which are really JField(name:String, value:JValue that is effectively String)
      parseOpt(values).
         fold(Seq.empty[(String, String)]) {
            _.children.collect { case JField(name, JString(value)) => (name, value) }
            // _.children.collect { case JObject(List(JField(name, JString(value)))) => (name, value) }
        }
    }
  }
}
