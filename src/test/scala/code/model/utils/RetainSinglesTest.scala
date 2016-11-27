package code.model.utils

import code.model.utils.RetainSingles._
import org.scalatest.{FlatSpec, Matchers}
import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-05-12.
  */
class RetainSinglesTest extends FlatSpec with Matchers {

  case class Model(k: Long, first: String, last : String, salary: Double) extends KeyHolder {
    override def getKey: String = k.toString
  }

  behavior of "empty"
  it should s"return empty sequence on empty input" in {
    val empty = Seq.empty[Model]
    empty.retainSingles.toStream shouldBe empty // have size 0
  }

  behavior of "repeats"
  val Philanthropist = new Model(1, "William", "Gates", 1000.00)
  val Scrooge = new Model(2, "Scrooge", "Scraper", 1.00)
  val oddPair = Seq(Philanthropist, Scrooge)
  val many: Seq[Model] = Seq.fill(100)(oddPair).flatten
  it should s"return 2 on normal pair" in {
    oddPair.retainSingles.toStream should have size 2
  }
  it should s"return size 2 on duped pair" in {
    many.retainSingles.toStream should have size 2
  }

}
