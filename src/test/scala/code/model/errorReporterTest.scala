package code.model

import code.UnitTest
import net.liftweb.util.Helpers.tryo
import net.liftweb.common.Box

/**
  * Created by philippederome on 2016-04-11. Look down for "monad" at the end to understand Box.
  * Note we don't use intercept on exceptions here because we capture them functionally (there's only ONE throw in whole app, at ORMExecutor).
  */

class errorReporterTest extends UnitTest {
  class errorReporterClass extends ErrorReporter

  val instance = new errorReporterClass
  def errorFormatter( s: String,  suffix: String): String = s"$s...$suffix"

  def unitBox(body: => Unit): Box[Unit] = tryo(body)
  def intsBox(body: => Iterable[Int]): Box[Iterable[Int]] = tryo(body)

  behavior of "checkUnitErrors"
  it should "return error string including some nefarious java.lang.IllegalArgumentException substring" in {
    def unit = {
      throw new IllegalArgumentException("Dropped the BOMB or some pleasant/useful IT/Product context") // you poor chap!
    }
    instance.checkUnitErrors(unitBox(unit), errorFormatter) should include("Full(java.lang.IllegalArgumentException:") // and trace stuff along the way
  }
  
  it should "return empty string on harmless normal box" in {
    def unit = {
      val nearZero = 0.0001
      val x = 5/nearZero // near miss!
    }
    instance.checkUnitErrors(unitBox(unit), errorFormatter) shouldBe empty
  }

  it should "return a specific error containing ArithmeticException with something about zero on DIV BY 0" in {
    def unit =  { val x = 5/0 }// kaboom! Disjunctor to the rescue (a Lift one not a Scalaz one)!
    instance.checkUnitErrors(unitBox(unit), errorFormatter) should include("Full(java.lang.ArithmeticException: / by zero)")
  }

  behavior of "checkErrors"
  it should "return empty on normal Iterable[Int]" in {
    def ints = {
      val nearZeroInt = 1
      val x = 5/nearZeroInt
      Seq(x, 1,2,3)
    }
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY!" // this is hobby project, not corporate, professional, I have license for irony.
    val errCheck = instance.checkErrors(intsBox(ints), errorFormatter, appContextError)
    errCheck shouldBe empty
  }

  it should "return a specific error with ArithmeticException and not some appContextError" in {
    def ints =  {
      val x: Int = 5/0
      Seq(x, 1,2,3)
    }
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY!" // this is hobby project, not corporate, professional, I have license for irony.
    instance.checkErrors(intsBox(ints), errorFormatter, appContextError) should equal("/ by zero...Full(java.lang.ArithmeticException: / by zero)")
  }

  it should "return app context message on unexpected empty collection" in {
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY!"
    instance.checkErrors(intsBox{ IndexedSeq()}, errorFormatter, appContextError) should equal(appContextError)
  }

  behavior of "Box monad"
  it should "return numbers as sequence of size 1 on normal Iterable[Int] after successful checkErrors" in {
    def ints = {
      val nearZeroInt = 1
      val x = 5/nearZeroInt
      Seq(x)
    }
    val monad = intsBox(ints)
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY!" // this is hobby project, not corporate, professional, I have license for irony.
    val errCheck = instance.checkErrors(monad, errorFormatter, appContextError)
    assert(errCheck.isEmpty)
    val numbers = for (x <- monad) yield x  // Monad usage (flatMap)
    numbers.map(identity).getOrElse(1 to 10) should have size 1  // not 10!
  }
}