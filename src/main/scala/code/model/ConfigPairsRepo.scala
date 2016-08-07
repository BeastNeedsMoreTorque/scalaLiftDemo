package code.model

import net.liftweb.json.parse
import net.liftweb.util.Props
import scala.collection.Seq
/**
  * Created by philippederome on 2016-04-08.
  */

/**
  * interface to get a sequence of key values that are related by a masterKey.
  */
trait ConfigPairsRepo {
  def getSeq(masterKey: String, default: String = ""): Seq[(String, String)]
}

object ConfigPairsRepo { // lo and behold, a module!!! ;-)  Arguably, excessive design for such a small app, but it shows the way.
  implicit def configPairsRepoPropsImpl: ConfigPairsRepo = new propsSeqReader // client wants this specific one
  implicit val defaultInstance = new propsSeqReader // client does not care about which one
  // provide as many implementations as required.
  class propsSeqReader extends ConfigPairsRepo {
    override def getSeq(masterKey: String, default: String = ""): Seq[(String, String)] = {
      val json = parse(Props.get(masterKey, default) )
      val pairs = for {elem <- json.children} yield elem.values // contains List of (String, JString(String))
      pairs.collect { case (pair: (String, String)) => pair }
    }
  }
}
