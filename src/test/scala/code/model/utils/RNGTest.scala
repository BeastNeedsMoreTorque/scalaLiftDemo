package code.model.utils

import scala.util.Random
import RNG._
import State._
import code.UnitTest
import code.SlowTest
import scala.language.reflectiveCalls

/**
  * Created by philippederome on 2016-04-28.
  * Unit testing is useful: it helped me identify stack overflow on shuffle for large N and later on rather egregious performance in my original
  * greedy, naive algorithm.
  * If performance is really very important, go imperative as per below:
  * @see Reservoir Sampling in Scala done by Spark: https://github.com/apache/spark/blob/master/core/src/main/scala/org/apache/spark/util/random/SamplingUtils.scala
  * It is proof enough that it's far from trivial and that ultimately when it comes to performance, what matters is proper algorithm (and data structures).
  *
  * The point of this exercise is practice FP, state, action handling to convert side-effect API to FP and use Scalatest.
  */
class RNGTest extends UnitTest {

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
  private def testNonNegativeInt(n: Int) = nonNegativeInt.run(Simple(n))._1 should be >= 0
  it should "work for corner cases" in testCornerCases(testNonNegativeInt)

  val seed0 = 10
  val r0 = Simple(seed0)
  val sampleSize = 5000
  val (samples, _) = sequence(List.fill(sampleSize)(nonNegativeInt)).run(r0)
  it should s"return only actual non-negatives on 'large' input when setting specific Simple" in {
    every(samples) should be >= 0  // uses Scalatest matchers (every)
  }

  it should s"check that sample size of results has no repeats (same as sampleSize $sampleSize)" in {
    samples.toSet.size shouldBe sampleSize
  }

  behavior of "Shuffle"
  // Actual values of Expected Shuffled values (exp_shuffled) are more to show the deterministic nature of the tests (pure functions using fixed seed)
  // In actual fact, a different implementation of the shuffle with same inputs could well be expected to yield different outputs.
  // So the check is more of a solution/function check than a BEHAVIOUR one.
  val shuffleOffset = 100
  val shuffleRange = (0 to 9).map( _ + shuffleOffset)
  it should s"predict correctly permutation of $shuffleRange when setting specific Simple" in {
    val seed1 = 20
    val (shuffled, _) = shuffle(shuffleRange).run(Simple(seed1))
    val expected_shuffled = List(106, 105, 101, 109, 100, 103, 102, 108, 104, 107)
    shuffled should equal(expected_shuffled)
  }

  it should s"predict correctly permutation of $shuffleRange when setting with other specific Simple" in {
    val seed2 = 10
    val (shuffled, _) = shuffle(shuffleRange).run(Simple(seed2))
    val expected_shuffled = List(109, 105, 100, 106, 101, 104, 108, 103, 107, 102)
    shuffled should equal(expected_shuffled)
  }

  it should s"return same element on permuting 1 element when setting with random seed" in new randomRNG with randomInt {
    val (shuffled, _) = shuffle(List(elem)).run(seed)
    val expected_shuffled = List(elem)
    shuffled should equal(expected_shuffled)
  }

  it should s"return empty sequence on permuting 0 element when setting with random seed" in new randomRNG {
    val (shuffled, _) = shuffle(Nil).run(seed)
    shuffled shouldBe empty
  }

  // This used to do stack overflow at about 2000-3000 items, which was NOT FUN AT ALL!
  // Still about 15-30 times slower than the official Random.shuffle one on 5000 items (mine is about 1.0 sec compared to 25-70 ms)
  // Warning: moderately slow.
  it should s"return sequence with no duplicates on permuting large sequence with random seed" taggedAs(SlowTest) in new randomRNG {
    val N = 10000
    val (shuffled, _) = shuffle(1 to N).run(seed)
    shuffled should have size N
  }

  behavior of "CollectSample"
  val bigSample = 1 to 5000
  it should s"predict pick 20 distinct elements from 1 to 5000 exactly on specific seed Simple" in {
    val k1 = 20
    val seed3 = 50
    val (selected, newSimple) = collectSample(bigSample, k1).run(Simple(seed3))
    val expected_shuffled = List(3524, 520, 3544, 4675, 3129, 121, 2005, 736, 3879, 3519, 3602, 3640, 2800, 2914, 2664, 4496, 4779, 4682, 895, 3121)
    (selected, newSimple) should equal(expected_shuffled, Simple(209042601209190L))
  }

  it should "get empty Sequence on choosing 0 item from non empty list with any random input" in new randomRNG {
    val (seq, _) = collectSample(bigSample, 0).run(seed)
    seq shouldBe empty
  }

  it should "get empty Sequence on choosing 10 items from empty list with any random input" in new randomRNG {
    val (seq, _) = collectSample(Seq[Int](), 10).run(seed)
    seq shouldBe empty
  }

}
