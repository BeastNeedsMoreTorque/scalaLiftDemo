package code.model.utils

// Copyright (c) 2012, Manning Publications, Co.

// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
// and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Edited and modified by Phil Derome, abiding by above copyright from Manning Publications, Co. Modified Software is provided "AS IS" as per above as well.

// Apparently, this State,Action and combinator idea is in line of thought of Scalaz (authors probably contributed to Scalaz).
// So I ported it to successor of Scalaz, i.e. Cats State.

// This code is more of a showcase of State, Action then being real pragmatic alternative to scala Random class/trait (no such aim to outdo it).
import scala.annotation.tailrec
import cats.data.State

trait RNG {
  def nextInt: (RNG, Int) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  type Rand[A] = State[RNG, A]
  case class Simple(seed: Long) extends RNG {
    def nextInt: (RNG, Int) = {
      val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG: RNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (nextRNG, n) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  val int: Rand[Int] = State(_.nextInt)

  def unit[A](a: A): Rand[A] =
    State( rng => (rng, a))

  def nonNegativeInt: Rand[Int] = State(rng => {
    val (r, i) = rng.nextInt
    val ii = if (i >= 0) i else -1 * (i + 1)
    (r, ii)
  })

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    State(rng => {
      val (rng2, a) = s.run(rng).value
      (rng2, f(a))
    })

  val double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def nonNegativeLessThan(n: Int): Rand[Int] = State(rng => {
    val (ra, a) = nonNegativeInt.run(rng).value
    @tailrec
    def go(i: Int)(rng: RNG): (RNG, Int) = {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (rng, mod) else go(n)(rng) // the retry prevents sample bias.
    }
    go(a)(ra)
    // map(nonNegativeInt)(_ % n) is incorrect due to sample bias
  })

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State(rng => {
    val (ra, a) = f.run(rng).value
    g(a).run(ra).value
  })

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    State(rng => {
      val (raa, a) = ra.run(rng).value
      val (rc, b) = rb.run(raa).value
      (rc, f(a, b))
    })


  // Algorithm S Sampling by Knuth (Volume 2, Section 3.4.2)
  def collectSample[T](s: Seq[T], k: Int): Rand[Seq[T]] = {
    // Don Knuth: Algorithm S (Selection sampling technique) N: total size, t visited (visiting item t+1), m selected, n requested/desired.
    // avail is assumed to be the numbers we select from randomly
    @tailrec
    def sampleIter(N: Int, n: Int, m: Int, t: Int, avail: Vector[T], selected: Seq[T], rng: RNG): (RNG, Seq[T]) =  {
      val (y, u) = double.run(rng).value
      // Knuth's method is not "functional" here (does not return a state capturing RNG) otherwise it's conceptually the same
      val sampleSuccess = (N - t) * u <= n - m
      if (sampleSuccess && m + 1 == n) {
        (y, selected ++ Seq(avail(t)))
      }
      else {
        // do this "trick" to satisfy tailrec.
        val (newSel, newM) = if (sampleSuccess) (selected ++ Seq(avail(t)), m + 1) else (selected, m)
        sampleIter(N, n, newM, t + 1, avail, newSel, y)
      }
    }

    State(rng => {
      if (s.isEmpty || k <= 0) {
        (rng, Seq())
      }  // allow client not to check for empty sequence (or specify k <= 0), robust and flexible.
      else {
        sampleIter(s.size, k, 0, 0, s.toVector, Seq(), rng)
      }
    })
  }

  // Algorithm P Shuffling by Knuth (Volume 2, Section 3.4.2). Attributed to Fisher-Yates
  // View the permutation selection as deck of cards shuffling game, which is how it got discovered in the 1930s.
  // Shuffle from last card to first in N steps by selecting another card lower in deck randomly to swap with.
  def shuffle( s: Seq[Int]): Rand[Seq[Int]] = {
    def swap[T](xs: Array[T], i: Int, j: Int) = {
      val t = xs(i)
      xs(i) = xs(j)
      xs(j) = t
    }

    State(rng => {
        val a = s.toArray
        var rr = rng
        for {j <- a.indices.reverse.dropRight(1)} { // without the reverse, we'd need a function to select in range [j N] for index k
          val (y, k) = nonNegativeLessThan(j).run(rr).value
          rr = y
          swap(a, k, j)
        }
        (rr, a)
    })
  }

  def randomElement(s: Seq[Int]): Rand[Seq[Int]] =
    flatMap(nonNegativeLessThan(s.size)) { i => unit(Seq(s(i))) }
}
