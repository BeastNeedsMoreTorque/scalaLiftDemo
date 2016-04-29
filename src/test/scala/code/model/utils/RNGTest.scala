package code.model.utils

import org.scalatest.{FlatSpec,ShouldMatchers}
import scala.util.Random
import RNG._
/**
  * Created by philippederome on 2016-04-28.
  */
class RNGTest extends FlatSpec with ShouldMatchers {
  val r = Simple(10)
  val sampleSize = 100
  val samples = sequence(List.fill(sampleSize)(nonNegativeInt))(r)

  val allPositives = samples._1.forall(_ >= 0)
  "checkNonNegative" should s"return true on AllPositives for $sampleSize" in {
    allPositives should equal(true)
  }

  val allTheSame = samples._1.toSet.size == 1
  "checkNonNegative" should s"return false on AllTheSame for $sampleSize" in {
    allTheSame should equal(false)
  }

  val (shuffled10_20, _) = shuffle(100 to 109)(Simple(20))
  val exp_shuffled10_20 = List(107, 106, 108, 101, 105, 104, 103, 102, 109, 100)
  "checkShuffle" should "predict permutation of 100 to 109 when setting Simple(20)" in {
    shuffled10_20 should equal(exp_shuffled10_20)
  }

  // Change the seed from 20 to 10, expect a different distinct permutation.
  val (shuffled10_10, _) = shuffle(100 to 109)(Simple(10))
  val exp_shuffled10_10 = List(100, 108, 101, 105, 107, 109, 102, 106, 103, 104)
  "checkShuffle" should "predict permutation of 100 to 109 when setting Simple(10)" in {
    shuffled10_10 should equal(exp_shuffled10_10)
  }

  val bigSample = 1 to 5000
  val exp_selected = Seq(3524, 520, 3544, 4675, 3129)
  val (selected, newSimple) = collectSample(bigSample, 5)(Simple(50))
  "checkCollectSample" should "predict pick 5 distinct elements from 1 to 5000 exactly as follows on seed Simple(50)" in {
    selected should equal(exp_selected)
  }
  "checkCollectSample" should "predict transform new Simple to expected from seed Simple(50)" in {
    newSimple should equal(Simple(239178062524073L))
  }

  val (emptySeq, _) = collectSample(bigSample, 0)(Simple(Random.nextInt()))
  "checkCollectSample" should "get empty Seq on choosing 0 item from non empty list with any random input" in {
    emptySeq should equal(Seq())
  }

  val (emptySeq2, _) = collectSample(Seq[Int](), 10)(Simple(Random.nextInt()))
  "checkCollectSample" should "get empty Seq on choosing 10 items from empty list with any random input" in {
    emptySeq2 should equal(Seq())
  }
}
