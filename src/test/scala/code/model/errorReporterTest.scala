package code.model

import java.sql.SQLException

import code.UnitTest
import scala.util.Try
import net.liftweb.common.Box
import net.liftweb.json.MappingException

/**
  * Created by philippederome on 2016-04-11. Look down for "monad" at the end to understand Box.
  * Note we don't use intercept on exceptions here because we capture them functionally (there's only ONE throw in whole app, at ORMExecutor).
  */

class errorReporterTest extends UnitTest {
  class errorReporterClass extends ErrorReporter

  val instance = new errorReporterClass
  def errorFormatter(suffix: String): String = s"...$suffix"

  def unitTry(body: => Unit): Try[Unit] = Try(body)
  def intsTry(body: => Iterable[Int]): Try[Iterable[Int]] = Try(body)


  behavior of "checkUnitErrors"
  // Please note the absence of try catch blocks.
  private def testExceptions[T >: Exception](test: (T, String) => Unit) = {
    val tests = Table(
      ("ex: Exception", "msg: String"), // The following are NOT The Magnificent Seven nor The Fab Four.
      (new IllegalArgumentException("Consider Partial Functions as with Actors"), "...java.lang.IllegalArgumentException: Consider Partial Functions as with Actors"),
      (new NullPointerException("Consider using Scala and Options"), "...java.lang.NullPointerException: Consider using Scala and Options"),
      (new ArithmeticException("was this a bad case of DIV BY 0?"), "...java.lang.ArithmeticException: was this a bad case of DIV BY 0?"),
      (new SQLException("Is the developer or DBA forgetful? Brittle data validation? access control or referential integrity issue?"), "...java.sql.SQLException: Is the developer or DBA forgetful? Brittle data validation? access control or referential integrity issue?"),
      (new MappingException("Consider extractOpt?"), "...net.liftweb.json.MappingException: Consider extractOpt?"))
    forAll(tests)(test)
  }

  private def testCaptureException(ex: Exception, msg: String) = instance.checkUnitErrors(unitTry({throw ex}), errorFormatter) should include(msg)
  it should "work for miscellaneous thrown exceptions" in testExceptions(testCaptureException)

  it should "return empty string on harmless normal box" in {
    def unit = {
      val nearZero = 0.0001
      val x = 5/nearZero // near miss!
    }
    instance.checkUnitErrors(unitTry(unit), errorFormatter) shouldBe empty
  }


  behavior of "checkErrors"

  it should "return empty on normal Iterable[Int]" in {
    def ints = {
      val nearZeroInt = 1
      val x = 5/nearZeroInt
      Seq(x, 1,2,3)
    }
    val appContxtError = "YOUR Integer COLLECTION IS EMPTY BECAUSE of too tight of a filter perhaps? TERRIBLY SORRY! ErrNo:654321, Sev:16" //
    val errCheck = instance.checkErrors(intsTry(ints), errorFormatter, appContxtError)
    errCheck shouldBe empty
  }

  it should "return a specific error with ArithmeticException and not some app contxt error" in {
    def ints =  {
      val x: Int = 5/0
      Seq(x, 1,2,3)
    }
    val appContxtError = "YOUR Integer COLLECTION IS EMPTY BECAUSE of too tight of a filter perhaps? TERRIBLY SORRY! ErrNo:123456, Sev: 18"
    instance.checkErrors(intsTry(ints), errorFormatter, appContxtError) should startWith("...java.lang.ArithmeticException: / by zero")  // Ouch!!
  }

  it should "return app context message on unexpected empty collection" in {
    val appContxtError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY! ErrNo:911, Sev: 4"
    instance.checkErrors(intsTry{ IndexedSeq()}, errorFormatter, appContxtError) should equal(appContxtError)
  }



  behavior of "Box monad"

  it should "return numbers as sequence of size 1 via for comprehension after successful checkErrors as not being empty (error case)" in {
    def ints = {
      val nearZeroInt = 1
      val x = 5/nearZeroInt
      Seq(x)
    }
    val monad = intsTry(ints)
    val appContextError = "YOUR Integer COLLECTION IS EMPTY BECAUSE OF USER TRAINING ISSUE, TERRIBLY SORRY! ErrNo:223344, Sev: 1024" // this is hobby project, not corporate, professional, I have license for irony.
    val errCheck = instance.checkErrors(monad, errorFormatter, appContextError)
    assert(errCheck.isEmpty)  // many ways to skin a cat (e.g. errCheck shouldBe empty)
    val numbers = for (x <- monad) yield x  // Monad usage (flatMap)
    numbers.map(identity).getOrElse(Seq()) should contain (5)
  }
}