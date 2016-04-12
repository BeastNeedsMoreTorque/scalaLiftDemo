package code.model

import org.scalatest.{FlatSpec,ShouldMatchers}

/**
  * Created by philippederome on 2016-04-11. It shows I prefer coding to testing... But code had to stabilize first too.
  */
class propsSeqReaderTest extends FlatSpec with ShouldMatchers {
  class propsSeqReaderClass extends propsSeqReader

  val instance = new propsSeqReaderClass
  var propsKey = "product.shortCategories"
  var propsSeq = Seq(("wine","Wine"),("spirits","Spirits"),("beer","Beer"))
  "getSeq" should "return sequence of categories" in {
    instance.getSeq(propsKey) should equal(propsSeq)
  }

  propsKey = "invalidKey"
  propsSeq = Seq()
  "getSeq" should "return empty sequence" in {
    instance.getSeq(propsKey) should equal(propsSeq)
  }
}


