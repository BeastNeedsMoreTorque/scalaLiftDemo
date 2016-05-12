package code.model.utils

import code.UnitTest

/**
  * Created by philippederome on 2016-05-12.
  */
class RetainSinglesTest  extends UnitTest {

  type Key = Long
  case class Model(k: Key, first: String, last : String, salary: Double) {}
  val modelToKey = {m: Model => m.k}

  behavior of "empty"
  it should s"return empty sequence on empty input" in {
    val empty = Seq.empty[Model]
    RetainSingles.filter(empty, modelToKey).toStream shouldBe empty //have size 0
  }

  behavior of "repeats"
  val Philanthropist = new Model(1, "William", "Gates", 1000.00)
  val Scrooge = new Model(2, "Scrooge", "Scraper", 1.00)
  val oddPair = Seq(Philanthropist, Scrooge)
  it should s"return 2 on normal pair" in {
    RetainSingles.filter(oddPair, modelToKey).toStream should have size 2
  }
  it should s"return 2 on duped pair" in {
    val many: Seq[Model] = Seq.fill(100)(oddPair).flatten
    RetainSingles.filter(oddPair, modelToKey).toStream should have size 2
  }
  it should s"return 3 on sum of distinct keys for many pairs" in {
    val many: Seq[Model] = Seq.fill(100)(oddPair).flatten
    RetainSingles.filter(oddPair, modelToKey).toStream.map(_.k).sum should === (3)
      }

  it should s"return 1 on first key of many duped pairs" in {
    val many: Seq[Model] = Seq.fill(100)(oddPair).flatten
    RetainSingles.filter(oddPair, modelToKey).toStream.map(_.k).head should === (1)
  }
  it should s"return 2 on last key of many duped pairs" in {
    val many: Seq[Model] = Seq.fill(100)(oddPair).flatten
    RetainSingles.filter(oddPair, modelToKey).toStream.map(_.k).last should === (2)
  }
}
