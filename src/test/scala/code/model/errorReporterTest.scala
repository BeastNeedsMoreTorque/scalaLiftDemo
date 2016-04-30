package code.model

import java.sql.SQLException

import code.UnitTest
import net.liftweb.util.Helpers.tryo
import net.liftweb.common.Box
import net.liftweb.json.MappingException

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
  // Please note the absence of try catch blocks.
  private def testExceptions[T >: Exception](test: (T, String) => Unit) = {
    val tests = Table(
      ("ex: Exception", "msg: String"), // The following are NOT The Magnificent Seven nor The Fab Four.
      (new IllegalArgumentException("Consider Partial Functions as with Actors"), "as with Actors...Full(java.lang.IllegalArgumentException:"),
      (new NullPointerException("Consider using Scala and Options"), "Scala and Options...Full(java.lang.NullPointerException:"),
      (new ArithmeticException("was this a bad case of DIV BY 0?"), "DIV BY 0?...Full(java.lang.ArithmeticException"),
      (new SQLException("Is the developer or DBA forgetful? Brittle data validation? access control or referential integrity issue?"), "control or referential integrity issue?...Full(java.sql.SQLException"),
      (new MappingException("Consider extractOpt?"), "extractOpt?...Full(net.liftweb.json.MappingException"))
    forAll(tests)(test)
  }

  private def testCaptureException(ex: Exception, msg: String) = instance.checkUnitErrors(unitBox({throw ex}), errorFormatter) should include(msg)
  it should "work for miscellaneous thrown exceptions" in testExceptions(testCaptureException)

  it should "return empty string on harmless normal box" in {
    def unit = {
      val nearZero = 0.0001
      val x = 5/nearZero // near miss!
    }
    instance.checkUnitErrors(unitBox(unit), errorFormatter) shouldBe empty
  }


  behavior of "checkErrors"

  it should "return empty on normal Iterable[Int]" in {
    def ints = {
      val nearZeroInt = 1
      val x = 5/nearZeroInt
      Seq(x, 1,2,3)
    }
    val appContxtError = "YOUR Integer COLLECTION IS EMPTY BECAUSE of too tight of a filter perhaps? TERRIBLY SORRY! ErrNo:654321, Sev:16" // this is hobby project, not corporate, professional, I have license for irony.
    val errCheck = instance.checkErrors(intsBox(ints), errorFormatter, appContxtError)
    errCheck shouldBe empty
  }

  it should "return a specific error with ArithmeticException and not some app contxt error" in {
    def ints =  {
      val x: Int = 5/0
      Seq(x, 1,2,3)
    }
    val appContxtError = "YOUR Integer COLLECTION IS EMPTY BECAUSE of too tight of a filter perhaps? TERRIBLY SORRY! ErrNo:123456, Sev: 18" // this is hobby project, not corporate, professional, I have license for irony.
    instance.checkErrors(intsBox(ints), errorFormatter, appContxtError) should equal("/ by zero...Full(java.lang.ArithmeticException: / by zero)") // Ouch!!
  }

  it should "return app context message on unexpected empty collection" in {
    val appContxtError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY! ErrNo:911, Sev: 4S"
    instance.checkErrors(intsBox{ IndexedSeq()}, errorFormatter, appContxtError) should equal(appContxtError)
  }



  behavior of "Box monad"

  it should "return numbers as sequence of size 1 on normal Iterable[Int] after successful checkErrors" in {
    def ints = {
      val nearZeroInt = 1
      val x = 5/nearZeroInt
      Seq(x)
    }
    val monad = intsBox(ints)
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY! ErrNo:223344, Sev: 1024" // this is hobby project, not corporate, professional, I have license for irony.
    val errCheck = instance.checkErrors(monad, errorFormatter, appContextError)
    assert(errCheck.isEmpty)
    val numbers = for (x <- monad) yield x  // Monad usage (flatMap)
    numbers.map(identity).getOrElse(1 to 10) should have size 1  // not 10!
  }
}