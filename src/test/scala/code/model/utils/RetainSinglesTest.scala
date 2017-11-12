package code.model.utils

import org.scalatest.{FlatSpec, Matchers}
import scala.language.implicitConversions
import RetainSingles._

/**
  * Created by philippederome on 2016-05-12.
  */
class RetainSinglesTest extends FlatSpec with Matchers {

  case class Model(k: Long, first: String, last : String, salary: Double)
  implicit val modelShowKey = new ShowKey[Model] {
    def show(f: Model) = 0.toLong
  }
  behavior of "empty"
  it should s"return empty sequence on empty input" in {
    val empty = Seq.empty[Model]
    retainSingles(empty).toStream shouldBe empty // have size 0
  }

  behavior of "repeats"
  val Philanthropist = new Model(1, "William", "Gates", 1000.00)
  val Scrooge = new Model(2, "Scrooge", "Scraper", 1.00)
  val oddPair = Seq(Philanthropist, Scrooge)
  val many: Seq[Model] = Seq.fill(100)(oddPair).flatten
  it should s"return 2 on normal pair" in {
    retainSingles(oddPair).toStream should have size 2
  }
  it should s"return size 2 on duped pair" in {
    retainSingles(many).toStream should have size 2
  }

}
