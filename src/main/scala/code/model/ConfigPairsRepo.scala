package code.model

import net.liftweb.json._
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
      val vals = for (elem <- json.children) yield elem.values // contains List of (String, JString(String))
      vals.map(_.asInstanceOf[(String, String)]) // could throw if contents that are configured are in wrong format
      // (violating assumption of pairs (String,String)...

      // Compiler cannot know I am putting Strings in the JSON data as I don't bother to use keys in data to be associated with a case class,
      // so trust me with the dirty cast.
      // @see http://ochafik.com/blog/?p=393 interesting alternative but would yield type erasure warning in this case:
      // elements collect { case se: SpecialElement if accept(se) => transform(se) } would become (l.collect { case x: (String, String) => x }).toMap
      // Later note, Feb 22: I convert all warnings to errors, so I should not get type erasure warnings anymore.
    }
  }

}
