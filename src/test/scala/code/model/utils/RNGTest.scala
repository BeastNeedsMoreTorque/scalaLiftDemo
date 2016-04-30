package code.model.utils

import scala.util.Random
import org.junit.runner.RunWith
import RNG._
import State._
import code.UnitTest
import code.SlowTest


/**
  * Created by philippederome on 2016-04-28.
  * Unit testing is useful: it helped me identify stack overflow on shuffle for large N and later on rather egregious performance in my original
  * greedy, naive algorithm.
  */
@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RNGTest extends UnitTest {

  behavior of "NonNegative"
  val seed0 = 10
  val r = Simple(seed0)
  val sampleSize = 100
  val (samples, _) = sequence(List.fill(sampleSize)(nonNegativeInt)).run(r)
  it should s"return only actual negatives on 'large' input (size $sampleSize) when setting Simple($seed0)" in {
    val allNonNegatives = samples.forall(_ >= 0)
    allNonNegatives shouldBe true
  }

  it should s"check that sample size of results has no repeats (same as sampleSize $sampleSize)" in {
    val sampleResSize = samples.toSet.size
    sampleResSize shouldBe sampleSize
  }

  behavior of "Shuffle"
  // Actual values of Expected Shuffled values (exp_shuffled) are more to show the deterministic nature of the tests (pure functions using fixed seed)
  // In actual fact, a different implementation of the shuffle with same inputs could well be expected to yield different outputs.
  // So the check is more of a solution/function check than a BEHAVIOUR one.
  val shuffleOffset = 100
  val shuffleRange = (0 to 9).map( _ + shuffleOffset)
  val seed1 = 20
  it should s"predict correctly permutation of $shuffleRange when setting Simple($seed1)" in {
    val (shuffled, _) = shuffle(shuffleRange).run(Simple(seed1))
    val exp_shuffled = List(109, 106, 102, 105, 101, 104, 103, 107, 100, 108)
    shuffled should equal(exp_shuffled)
  }

  val seed2 = 10
  it should s"predict correctly permutation of $shuffleRange when setting with other seed Simple($seed2)" in {
    val (shuffled, _) = shuffle(shuffleRange).run(Simple(seed2))
    val exp_shuffled = List(105, 106, 101, 109, 102, 100, 107, 104, 108, 103)
    shuffled should equal(exp_shuffled)
  }

  it should s"return same element on permuting 1 element when setting with random seed" in {
    val elem = Random.nextInt()
    val seed = Random.nextInt()
    val (shuffled, _) = shuffle(List(elem)).run(Simple(seed))
    val exp_shuffled = List(elem)
    shuffled should equal(exp_shuffled)
  }

  it should s"return empty sequence on permuting 0 element when setting with random seed" in {
    val seed = Random.nextInt()
    val exp_shuffled = List()
    val (shuffled, _) = shuffle(exp_shuffled).run(Simple(seed))
    shuffled should equal(exp_shuffled)
  }

  // This used to do stack overflow at about 2000-3000 items, which was NOT FUN AT ALL!
  // Still about 4-7 times slower than the official Random.shuffle one on 10000 items (mine is 1.0 sec to 1.7 sec compared to 246 ms)
  // Warning: moderately slow.
  it should s"return sequence with no duplicates on permuting large sequence with random seed" taggedAs(SlowTest) in {
    val seed = Random.nextInt()
    val N = 10000
    val (shuffled, _) = shuffle(1 to N).run(Simple(seed))
    shuffled should have size N
  }

  behavior of "CollectSample"

  val bigSample = 1 to 5000
  val k1 = 5
  val seed3 = 50
  val (selected, newSimple) = collectSample(bigSample, k1).run(Simple(seed3))
  val exp_selected = Seq(3524, 520, 3544, 4675, 3129)
  it should s"predict pick $k1 distinct elements from 1 to 5000 exactly as ($exp_selected, Simple(239178062524073)) on seed Simple($seed3)" in {
    (selected, newSimple) should equal((exp_selected), Simple(239178062524073L))
  }

  it should "get empty Sequence on choosing 0 item from non empty list with any random input" in {
    val (seq, _) = collectSample(bigSample, 0).run(Simple(Random.nextInt()))
    seq shouldBe empty
  }

  it should "get empty Sequence on choosing 10 items from empty list with any random input" in {
    val (seq, _) = collectSample(Seq[Int](), 10).run(Simple(Random.nextInt()))
    seq shouldBe empty
  }

}
