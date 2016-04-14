package code.model

import org.scalatest.{FlatSpec,ShouldMatchers}
import propsSeqReader.getSeq

/**
  * Created by philippederome on 2016-04-11. It shows I prefer coding to testing... But code had to stabilize first too.
  */
class propsSeqReaderTest extends FlatSpec with ShouldMatchers {
  class propsSeqReaderClass

  var propsKey = "product.shortCategories"
  var propsSeq = Seq(("wine","Wine"),("spirits","Spirits"),("beer","Beer"))
  "getSeq" should "return sequence of categories" in {
    getSeq(propsKey) should equal(propsSeq)
  }

  propsKey = "invalidKey"
  propsSeq = Seq()
  "getSeq" should "return empty sequence" in {
    getSeq(propsKey) should equal(propsSeq)
  }
}


