package code.model

import net.liftweb.util.Props
import io.circe.parser.{parse, _}
import io.circe._
import io.circe.generic.auto._
import cats.syntax.either._

/**
  * Created by philippederome on 2016-04-08.
  * A module offering several possible different implementations of ConfigPairsRepo
  */
object ConfigPairsRepo {
  case class KV(k: String, v: String) // to assist JSON parsing
  val emptyString: String = ""
  val emptyPairs = Seq.empty[(String, String)]
  /**
    * represents a map of String to Map(key:String, value:String) that is configurable at run time.
    * @param masterKey a key to some configuration holding a map of values
    * @return a map of strings to strings bound to the masterKey
    */
  def getSeq(masterKey: String): Seq[(String, String)] = {
    // call to Props.get assumed to yield {"key1":"value1",... "keyn":"valuen"}, which is JSON
    val doc = parse(Props.get(masterKey, emptyString)).getOrElse(Json.Null)
    (doc.hcursor.downField("list").as[Seq[KV]] match {
      case Left(_) => Nil
      case Right(xs) => xs
    }).map{ kv => (kv.k, kv.v) }
  }
}
