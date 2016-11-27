package code.model.utils

import scala.util.Random
import RNG._
import code.UnitTest
import code.SlowTest
import org.scalatest.prop.PropertyChecks

/**
  * Created by philippederome on 2016-04-28.
  * Unit testing is useful: it helped me identify stack overflow on shuffle for large N and later on rather egregious performance in my original
  * greedy, naive algorithm. It also helped optimize and refactor correctly.
  * Since performance is typically important, follow Don Knuth Book 2 on Numerical Analysis methods Section 3.4.
  *
  * The point of this exercise is practice FP, state, action handling to convert side-effect API to FP and use Scalatest.
  * Need more practice with Scalatest and writing test cases much more concisely.
  */
class RNGTest extends UnitTest with PropertyChecks {

  // @see http://www.scalatest.org/user_guide/sharing_fixtures (Instantiating fixture objects)
  // technique normally used to localize side effects to single test cases. A little overkill for what's done here, but demonstrates the technique
  // for more complex scenarios. Fixtures allow side effect clean up, not used here.
  trait randomRNG {
    val seed = Simple(Random.nextInt())
  }

  trait randomInt {
    val elem = Random.nextInt()
  }

  behavior of "NonNegative"
  private def testCornerCases(test: Int => Unit) = {
    val tests = Table(
      ("n: Int"),
      (0), (1), (-1), (Int.MaxValue), (Int.MinValue))
    forAll(tests)(test)
  }
  private def testNonNegativeInt(n: Int) = nonNegativeInt.runA(Simple(n)).value should be >= 0
  it should "work for corner cases" in testCornerCases(testNonNegativeInt)

  val seed0 = 10
  val r0 = Simple(seed0)
  val sampleSize = 5000
  val samples = sequence(List.fill(sampleSize)(nonNegativeInt)).runA(r0).value
  it should s"return only actual non-negatives on 'large' input when setting specific Simple" in {
    every(samples) should be >= 0  // uses Scalatest matchers (every)
  }

  it should s"check that sample size of results has no repeats (same as sampleSize $sampleSize)" in {
    samples.toSet.size shouldBe sampleSize
  }

  behavior of "ints"

  val randomInts = ints(sampleSize)(r0)._2
  it should s"check that sample size of results for positive/negative ints has no repeats (same as sampleSize $sampleSize)" in {
    randomInts.toSet.size shouldBe sampleSize
  }

  behavior of "Shuffle"
  // Actual values of Expected Shuffled values (exp_shuffled) are more to show the deterministic nature of the tests (pure functions using fixed seed)
  // In actual fact, a different implementation of the shuffle with same inputs could well be expected to yield different outputs.
  // So the check is more of a solution/function check than a BEHAVIOUR one.
  // It'd be better to use "properties" and generators instead as per ScalaCheck
  val shuffleOffset = 100
  val shuffleRange = (0 to 9).map( _ + shuffleOffset)
  it should s"predict correctly permutation of $shuffleRange when setting specific Simple" in {
    val seed1 = 20
    val shuffled = shuffle(shuffleRange).runA(Simple(seed1)).value
    val expected_shuffled = Array(103, 106, 107, 109, 102, 108, 104, 100, 101, 105)
    shuffled should equal(expected_shuffled)
  }

  // this one is not truly "behaviour". It's better to use "properties" and generators instead as per ScalaCheck
  it should s"predict correctly permutation of $shuffleRange when setting with other specific Simple" in {
    val seed2 = 10
    val shuffled = shuffle(shuffleRange).runA(Simple(seed2)).value
    val expected_shuffled = Array(102, 100, 105, 101, 103, 108, 109, 104, 106, 107)
    shuffled should equal(expected_shuffled)
  }

  it should s"return same element on permuting 1 element when setting with random seed" in new randomRNG with randomInt {
    val shuffled = shuffle(Vector(elem)).runA(seed).value
    val expected_shuffled = Vector(elem)
    shuffled should equal(expected_shuffled)
  }

  it should s"return empty sequence on permuting 0 element when setting with random seed" in new randomRNG {
    val shuffled = shuffle(Vector.empty[Int]).runA(seed).value
    shuffled shouldBe empty
  }

  // Warning: very moderately slow.
  it should s"return sequence with no duplicates on permuting large sequence with random seed" taggedAs(SlowTest) in new randomRNG {
    val N = 10000
    val shuffled = shuffle(1 to N).runA(seed).value
    shuffled.toSet should have size N
  }

  behavior of "CollectSample"
  val bigSample: Vector[Int] = (1 to 5000).toVector

  // this one is not truly "behaviour". It's better to use "properties" and generators instead as per ScalaCheck
  it should s"predict pick 20 distinct elements from 1 to 5000 exactly on specific seed Simple" in {
    val k1 = 20
    val seed3 = 50
    val (newSimple, selected) = collectSample(bigSample, k1).run(Simple(seed3)).value
    val expected_shuffled = List(107, 288, 472, 844, 867, 885, 1515, 1696, 1905, 2080, 2300, 2453, 2841, 3329, 3422, 3788, 4150, 4416, 4552, 4775)
    (selected, newSimple) should equal((expected_shuffled, Simple(281348911128875L)))
  }

  it should "get unit Sequence on choosing 10 items from a small list (1 item) with any random input" in new randomRNG {
    collectSample(Vector(5), 10).runA(seed).value shouldBe Vector(5)
  }

  it should "get empty Sequence on choosing 0 item from non empty list with any random input" in new randomRNG {
    collectSample(bigSample, 0).runA(seed).value shouldBe empty
  }

  it should "get empty Sequence on choosing 10 items from empty list with any random input" in new randomRNG {
    collectSample(Vector[Int](), 10).runA(seed).value shouldBe empty
  }

}
