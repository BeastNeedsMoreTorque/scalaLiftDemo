package code.model.utils

import scala.util.Random

import org.junit.runner.RunWith
import RNG._

/**
  * Created by philippederome on 2016-04-28.
  */
@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RNGTest extends UnitTest {

  val seed0 = 10
  val r = Simple(seed0)
  val sampleSize = 100
  val samples = sequence(List.fill(sampleSize)(nonNegativeInt))(r)

  behavior of "checkNonNegative"

  "checkNonNegative" should s"return only actual negatives on 'large' input (size $sampleSize) when setting Simple($seed0)" in {
    val allNonNegatives = samples._1.forall(_ >= 0)
    allNonNegatives shouldBe true
  }

  it should s"check that sample size of results has no repeats (same as sampleSize $sampleSize)" in {
    val sampleResSize = samples._1.toSet.size
    sampleResSize shouldBe sampleSize
  }

  behavior of "checkShuffle"

  val shuffleOffset = 100
  val shuffleRange = (0 to 9).map( _ + shuffleOffset)
  val seed1 = 20
  "checkShuffle" should s"predict correctly permutation of $shuffleRange when setting Simple($seed1)" in {
    val (shuffled, _) = shuffle(shuffleRange)(Simple(seed1))
    val exp_shuffled = List(104, 105, 107, 101, 108, 106, 103, 109, 102, 100)
    shuffled should equal(exp_shuffled)
  }

  val seed2 = 10
  it should s"predict correctly permutation of $shuffleRange when setting with other seed Simple($seed2)" in {
    val (shuffled, _) = shuffle(shuffleRange)(Simple(seed2))
    val exp_shuffled = List(102, 109, 100, 107, 105, 101, 108, 103, 106, 104)
    shuffled should equal(exp_shuffled)
  }

  it should s"return same element on permuting 1 element when setting with random seed" in {
    val elem = Random.nextInt()
    val seed = Random.nextInt()
    val (shuffled, _) = shuffle(List(elem))(Simple(seed))
    val exp_shuffled = List(elem)
    shuffled should equal(exp_shuffled)
  }

  it should s"return empty sequence on permuting 0 element when setting with random seed" in {
    val seed = Random.nextInt()
    val exp_shuffled = List()
    val (shuffled, _) = shuffle(exp_shuffled)(Simple(seed))
    shuffled should equal(exp_shuffled)
  }

  it should s"return sequence with no duplicates on permuting large sequence with random seed" in {
    val seed = Random.nextInt()
    val N = 4000
    val (shuffled, _) = shuffle(1 to N)(Simple(seed))
    shuffled should have size N
  }

  behavior of "checkCollectSample"

  val bigSample = 1 to 5000
  val k1 = 5
  val seed3 = 50
  val (selected, newSimple) = collectSample(bigSample, k1)(Simple(seed3))
  val exp_selected = Seq(3129, 4675, 3544, 520, 3524)
  "checkCollectSample" should s"predict pick $k1 distinct elements from 1 to 5000 exactly as ($exp_selected, Simple(239178062524073)) on seed Simple($seed3)" in {
    (selected, newSimple) should equal((exp_selected), Simple(239178062524073L))
  }

  it should "get empty Sequence on choosing 0 item from non empty list with any random input" in {
    val (seq, _) = collectSample(bigSample, 0)(Simple(Random.nextInt()))
    seq shouldBe empty
  }

  it should "get empty Sequence on choosing 10 items from empty list with any random input" in {
    val (seq, _) = collectSample(Seq[Int](), 10)(Simple(Random.nextInt()))
    seq shouldBe empty
  }

}
