package code.model.utils

import code.UnitTest
import code.model.utils.RetainSingles.removeDupesQuietly
import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-05-12.
  */
class RetainSinglesTest  extends UnitTest {

  case class Model(k: Long, first: String, last : String, salary: Double) extends KeyHolder {
    override def getKey = k.toString
  }

  behavior of "empty"
  it should s"return empty sequence on empty input" in {
    val empty = Seq.empty[Model]
    removeDupesQuietly(empty ).toStream shouldBe empty //have size 0
  }

  behavior of "repeats"
  val Philanthropist = new Model(1, "William", "Gates", 1000.00)
  val Scrooge = new Model(2, "Scrooge", "Scraper", 1.00)
  val oddPair = Seq(Philanthropist, Scrooge)
  val many: Seq[Model] = Seq.fill(100)(oddPair).flatten
  it should s"return 2 on normal pair" in {
    removeDupesQuietly(oddPair).toStream should have size 2
  }
  it should s"return size 2 on duped pair" in {
    removeDupesQuietly(many).toStream should have size 2
  }

  behavior of "preserving order after filtering"
  it should s"return 1 on first key of many duped pairs" in {
    removeDupesQuietly(many).toStream.map(_.getKey).head should === ("1")
  }
  it should s"return 2 on last key of many duped pairs" in {
    removeDupesQuietly(many).toStream.map(_.getKey).last should === ("2")
  }
}
