package code.model

import org.scalatest.{FlatSpec, ShouldMatchers}
import net.liftweb.util.Helpers.tryo
import net.liftweb.common.Box

/**
  * Created by philippederome on 2016-04-11.
  */

class errorReporterTest extends FlatSpec with ShouldMatchers {
  class errorReporterClass extends ErrorReporter

  val instance = new errorReporterClass

  def sweetAndShortErrorLogger( s: String,  suffix: String): String = s"$s...$suffix"
  val box1: Box[Unit] =  tryo {
      val x: Int = 0
      throw new IllegalArgumentException("Dropped the BOMB") // you poor chap!
  }

  "checkUnitErrors on exception" should "return false with IllegalArgumentException" in {
    instance.checkUnitErrors(box1, sweetAndShortErrorLogger) should equal("Dropped the BOMB...Full(java.lang.IllegalArgumentException: Dropped the BOMB)") // and trace stuff along the way
  }

  val box2 = tryo {
    val nearZero = 0.0001
    val x = 5/nearZero
  }
  "checkUnitErrors on harmless" should "return true" in {
    instance.checkUnitErrors(box2, sweetAndShortErrorLogger) should equal( "")
  }

  val box3 = tryo {
    val x = 5/0
  }
  "checkUnitErrors on DIV0" should "return false with ArithmeticException" in {
    instance.checkUnitErrors(box3, sweetAndShortErrorLogger) should equal("/ by zero...Full(java.lang.ArithmeticException: / by zero)")
  }

  def detailedErrorLogger( s: String,  suffix: String): String = s"$s...$suffix"
  var box4: Box[Iterable[Int]] = tryo {
    val x: Int = 5/0
    Seq(x, 1,2,3)
  }
  "checkErrors on DIV0 Exception" should "return false with ArithmeticException" in {
    instance.checkErrors(box4, detailedErrorLogger, "YOUR COLLECTION IS EMPTY, SORRY!") should equal("/ by zero...Full(java.lang.ArithmeticException: / by zero)")
  }

  val m = Map[Int, Iterable[Int]]()
  val box5: Box[Iterable[Int]] = tryo { IndexedSeq()}
  "checkErrors on unexpected empty collection" should "return false with given empty collection message" in {
    instance.checkErrors(box5, detailedErrorLogger, "YOUR COLLECTION IS EMPTY, SORRY!") should equal("YOUR COLLECTION IS EMPTY, SORRY!")
  }
}