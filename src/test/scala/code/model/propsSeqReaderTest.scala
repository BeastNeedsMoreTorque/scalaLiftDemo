package code.model

import code.UnitTest

/**
  * Created by philippederome on 2016-04-11.
  * Uses main/resources/test.default.props for Properties content.
  */
class propsSeqReaderTest extends UnitTest {

  behavior of "getSeq"

  it should "return empty sequence when masterKey is invalid" in {
    ConfigPairsRepo.getSeq("invalidKey") shouldBe empty
  }
}


