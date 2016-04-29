package code.model.utils

import org.scalatest.FlatSpec
import scala.util.Random
import RNG._

import org.junit.runner.RunWith
import org.scalacheck.Gen
import org.scalatest.Matchers
import org.scalatest.prop.PropertyChecks
import RNG._
import org.scalacheck.Commands
import org.scalatest.prop.Checkers

/**
  * Created by philippederome on 2016-04-28.
  */
@RunWith(classOf[org.scalatest.junit.JUnitRunner])
class RNGTest extends FlatSpec with PropertyChecks with Matchers {
  val r = Simple(10)
  val sampleSize = 100
  val samples = sequence(List.fill(sampleSize)(nonNegativeInt))(r)

  "checkNonNegative" should s"return true on AllPositives for $sampleSize" in {
    val allPositives = samples._1.forall(_ >= 0)
    allPositives shouldBe true
  }

  it should "check that sampleResSize  > 1" in {
    val sampleResSize = samples._1.toSet.size
    sampleResSize should be > 1
  }


  "checkShuffle" should "predict permutation of 100 to 109 when setting Simple(20)" in {
    val (shuffled10_20, _) = shuffle(100 to 109)(Simple(20))
    val exp_shuffled10_20 = List(107, 106, 108, 101, 105, 104, 103, 102, 109, 100)
    shuffled10_20 should equal(exp_shuffled10_20)
  }

  // Change the seed from 20 to 10, expect a different distinct permutation.
  it should "predict permutation of 100 to 109 when setting Simple(10)" in {
    val (shuffled10_10, _) = shuffle(100 to 109)(Simple(10))
    val exp_shuffled10_10 = List(100, 108, 101, 105, 107, 109, 102, 106, 103, 104)
    shuffled10_10 should equal(exp_shuffled10_10)
  }

  val bigSample = 1 to 5000
  val (selected, newSimple) = collectSample(bigSample, 5)(Simple(50))
  "checkCollectSample" should "predict pick 5 distinct elements from 1 to 5000 exactly as follows on seed Simple(50)" in {
    val exp_selected = Seq(3524, 520, 3544, 4675, 3129)
    selected should equal(exp_selected)
  }

  it should "predict transform new Simple to expected from seed Simple(50)" in {
    newSimple should equal(Simple(239178062524073L))
  }

  it should "get empty Seq on choosing 0 item from non empty list with any random input" in {
    val (seq, _) = collectSample(bigSample, 0)(Simple(Random.nextInt()))
    seq shouldBe empty
  }

  it should "get empty Seq on choosing 10 items from empty list with any random input" in {
    val (seq, _) = collectSample(Seq[Int](), 10)(Simple(Random.nextInt()))
    seq shouldBe empty
  }

}
