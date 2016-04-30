package code.model

import code.model.utils.UnitTest
import net.liftweb.util.Helpers.tryo
import net.liftweb.common.Box

/**
  * Created by philippederome on 2016-04-11. Look down for "monad" at the end to understand Box.
  */

class errorReporterTest extends UnitTest {
  class errorReporterClass extends ErrorReporter

  val instance = new errorReporterClass
  def errorFormatter( s: String,  suffix: String): String = s"$s...$suffix"

  behavior of "checkUnitErrors"
  "checkUnitErrors" should "return error string including some nefarious java.lang.IllegalArgumentException substring" in {
    val box: Box[Unit] =  tryo {
      throw new IllegalArgumentException("Dropped the BOMB or some pleasant/useful IT/Product context") // you poor chap!
    }
    instance.checkUnitErrors(box, errorFormatter) should include("Full(java.lang.IllegalArgumentException:") // and trace stuff along the way
  }
  
  it should "return empty string on harmless normal box" in {
    val box: Box[Unit] = tryo {
      val nearZero = 0.0001
      val x = 5/nearZero // near miss!
    }
    instance.checkUnitErrors(box, errorFormatter) shouldBe empty
  }

  it should "return a specific error containing ArithmeticException with something about zero on DIV BY 0" in {
    val box = tryo {
      val x = 5/0 // kaboom! Disjunctor to the rescue (a Lift one not a Scalaz one)!
    }
    instance.checkUnitErrors(box, errorFormatter) should include("Full(java.lang.ArithmeticException: / by zero)")
  }

  behavior of "checkErrors"
  "checkErrors" should "return empty on normal Iterable[Int]" in {
    val box: Box[Iterable[Int]] = tryo {
      val nearZeroInt = 1
      val x = 5/nearZeroInt
      Seq(x, 1,2,3)
    }
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY!" // this is hobby project, not corporate, professional, I have license for irony.
    val errCheck = instance.checkErrors(box, errorFormatter, appContextError)
    errCheck shouldBe empty
  }

  it should "return a specific error with ArithmeticException and not some appContextError" in {
    var box: Box[Iterable[Int]] = tryo {
      val x: Int = 5/0
      Seq(x, 1,2,3)
    }
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY!" // this is hobby project, not corporate, professional, I have license for irony.
    instance.checkErrors(box, errorFormatter, appContextError) should equal("/ by zero...Full(java.lang.ArithmeticException: / by zero)")
  }

  it should "return app context message on unexpected empty collection" in {
    val boxContainingEmpty: Box[Iterable[Int]] = tryo { IndexedSeq()}
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY!"
    instance.checkErrors(boxContainingEmpty, errorFormatter, appContextError) should equal(appContextError)
  }

  behavior of "Box monad"
  "monad demo" should "return numbers as sequence of size 1 on normal Iterable[Int] after successful checkErrors" in {
    val box: Box[Iterable[Int]] = tryo {
      val nearZeroInt = 1
      val x = 5/nearZeroInt
      Seq(x)
    }
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY!" // this is hobby project, not corporate, professional, I have license for irony.
    val errCheck = instance.checkErrors(box, errorFormatter, appContextError)
    errCheck shouldBe empty
    val numbers = for (x <- box) yield x  // Monad usage (flatMap)
    numbers.map(identity).getOrElse(1 to 10) should have size 1  // not 10!
  }
}