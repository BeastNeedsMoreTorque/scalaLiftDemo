package code.model

import code.model.utils.UnitTest

/**
  * Created by philippederome on 2016-04-11.
  * Uses main/resources/test.default.props for Properties content.
  */
class propsSeqReaderTest extends UnitTest {

  "getSeq" should "return exact sequence of categories (json parsed) keyed by valid masterKey" in {
    val keyVals = Seq(("wine","Wine"),("spirits","Spirits"),("beer","Beer"))
    ConfigPairsRepo.defaultInstance.getSeq("product.shortCategories") should equal(keyVals)
  }

  "getSeq" should "return empty sequence when masterKey is invalid" in {
    ConfigPairsRepo.defaultInstance.getSeq("invalidKey") shouldBe empty
  }
}


