package code.model.utils

import scala.util.Random

/**
  * EXPERIMENTAL. NOT READY FOR USE. DEAD CODE FOR NOW.
  * Created by philippederome on 2016-04-24. Consider making getBytes and getShorts pure functions as per Functional Programming in Scala, Chapter 6.
  * And then unit tests would be possible on that. Unclear yet as to whether replacing Random.shuffle with a pure function is simple enough.
  * Note that this is too experimental to be close to production ready.
  * TODO: the modulus upperBound introduces an unwanted bias in selection. This is well covered in FP in Scala Ch. 6.
  * Also the mapping from byte array to ranges to upper bound suffers from repeats. It'd much better to sample without repeats
  * (i.e. have a random sampler class that defines exactly the sample set and then sample once from it and then adjust the set;
  * sampling once from a set would mean take a number between 1 and N where N is cardinality of set and then find k-th largest element, yuk, that could be slow!)
  */
object Combinator {
  // may return less than count since the # of client's elements (upperBound) could be less than desired count.
  def sampleNonNegativeShorts(upperBound: Int, count: Int): Seq[Int] = {
    def getBytes(n: Int) = {
      if (n <= 0) sys.error("wrong argument")
      val bytes = new Array[Byte](n)
      Random.nextBytes(bytes)
      bytes
    }

    // gets n random 2 byte numbers (usually called shorts)
    def getShorts(n: Int) =
      getBytes(2*n).grouped(2).map(x => x(0) + 0x100*x(1)) // may return negatives.

    // Random.shuffle on indices is more appropriate if sample size is close to the upper bound, which we determine as sample size being half
    // of the upper bound somewhat arbitrarily.
    if (count < 1 || upperBound > 0x7FFF) sys.error("wrong argument") // not interested in sampling randomly over collections of size greater than 2*15 (hence "Shorts" in method name).
    val preShuffle = if (upperBound <= 2* count ) 0 until upperBound  // will permute over full range of a available numbers
    else getShorts(count).map(x => Math.abs(x % upperBound)).toSet // will permute over a small set of size count (set gives us uniqueness but could break randomness)

    Random.shuffle(preShuffle.toIndexedSeq).take(count) // shuffle makes no sense on sets since they don't have an order but it could run on them.
  }

}
