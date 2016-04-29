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

  "checkNonNegative" should s"return only actual negatives on 'large' input (size $sampleSize) when setting Simple($seed0)" in {
    val allNonNegatives = samples._1.forall(_ >= 0)
    allNonNegatives shouldBe true
  }

  it should s"check that sample size of results has no repeats (same as sampleSize $sampleSize)" in {
    val sampleResSize = samples._1.toSet.size
    sampleResSize shouldBe sampleSize
  }

  val shuffleOffset = 100
  val shuffleRange = (0 to 9).map( _ + shuffleOffset)
  val seed1 = 20
  "checkShuffle" should s"predict correctly permutation of $shuffleRange when setting Simple($seed1)" in {
    val (shuffled, _) = shuffle(shuffleRange)(Simple(seed1))
    val exp_shuffled = List(107, 106, 108, 101, 105, 104, 103, 102, 109, 100)
    shuffled should equal(exp_shuffled)
  }

  val seed2 = 10
  it should s"predict correctly permutation of $shuffleRange when setting with other seed Simple($seed2)" in {
    val (shuffled, _) = shuffle(shuffleRange)(Simple(seed2))
    val exp_shuffled = List(100, 108, 101, 105, 107, 109, 102, 106, 103, 104)
    shuffled should equal(exp_shuffled)
  }

  val bigSample = 1 to 5000
  val k1 = 5
  val seed3 = 50
  val (selected, newSimple) = collectSample(bigSample, k1)(Simple(seed3))
  val exp_selected = Seq(3524, 520, 3544, 4675, 3129)
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
