package code.model

import org.scalatest.{FlatSpec,ShouldMatchers}

/**
  * Created by philippederome on 2016-04-11. It shows I prefer coding to testing... But code had to stabilize first too.
  */
class propsSeqReaderTest extends FlatSpec with ShouldMatchers {
  self: ConfigPairsRepo => // cake pattern!

  var masterKey = "product.shortCategories"
  var keyVals = Seq(("wine","Wine"),("spirits","Spirits"),("beer","Beer"))
  "getSeq" should "return sequence of categories" in {
    getSeq(masterKey) should equal(keyVals)
  }

  masterKey = "invalidKey"
  keyVals = Seq()
  "getSeq" should "return empty sequence" in {
    getSeq(masterKey) should equal(keyVals)
  }
}


