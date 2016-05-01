package code.model.utils

//Copyright (c) 2012, Manning Publications, Co.

//Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

//The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Edited and modified by Phil Derome, abiding by above copyright from Manning Publications, Co. Modified Software is provided "AS IS" as per above as well.

// Apparently, this State,Action and combinator idea is in line of thought of Scalaz (authors probably contributed to Scalaz).

// This code is more of a showcase of State, Action, Combinator then being real pragmatic alternative to scala Random class/trait (no such aim to outdo it).
import scala.annotation.tailrec
import State._

import scala.collection.mutable.ArrayBuffer

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

}

object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed: Long = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG: RNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  val int: Rand[Int] = State(_.nextInt)

  def unit[A](a: A): Rand[A] =
   State( rng => (a, rng))

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    State(rng => {
      val (a, rng2) = s.run(rng)
      (f(a), rng2)
    })

  def nonNegativeInt: Rand[Int] = State(rng => {
    val (i, r) = rng.nextInt
    val ii = if (i >= 0) i else -1 * (i + 1)
    (ii, r)
  })

  val double: Rand[Double] =
    map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = State(rng => {
    val (a, ra) = f.run(rng)
    g(a).run(ra)
  })

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    State(rng => {
      val (a, raa) = ra.run(rng)
      val (b, rc) = rb.run(raa)
      (f(a, b), rc)
    })


  def ints(n: Int)(rng: RNG): (List[Int], RNG) =
    sequence(List.fill(n)(int)).run(rng)

  def nonNegativeLessThan(n: Int): Rand[Int] = State(rng => {
    val (a, ra) = nonNegativeInt.run(rng)
    @tailrec
    def go(i: Int)(rng: RNG): (Int, RNG) = {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) (mod, rng) else go(n)(rng)
    }
    go(a)(ra)
  })

  def randomElement(s: Seq[Int]): Rand[Seq[Int]] =
    flatMap(nonNegativeLessThan(s.size)) { i => unit(Seq(s(i))) }

  // Don Knuth: Algorithm S (Selection sampling technique) N: total size, t visited (visiting item t+1), m selected, n requested/desired.
  // avail is assumed to be the numbers we select from randomly
  def sampleIter[T](N: Int, n: Int, m: Int, t: Int, avail: Vector[T], selected: Seq[T], rng: RNG): (Seq[T], RNG) =  {
    val (u,y) = double.run(rng)
    if ((N-t)* u > n-m) {
      // sample failed (we may succeed next and last ones with certainty, nothing to test, it's when we succeed on last one that we're done)
      sampleIter(N, n, m, t+1, avail, selected, y )
    }  // sample succeeded
    else if (m + 1 == n) {
      // final success
      (selected ++ Seq(avail(t)), y)
    }
    else {
      sampleIter(N, n, m+1, t+1, avail, selected ++ Seq(avail(t)), y )
    }
  }

  def collectSampleKnuth[T](s: Seq[T], k: Int): Rand[Seq[T]] = State(rng =>  {
    if (s.isEmpty) (Seq(), rng)
    else sampleIter(s.size, k, 0, 0, s.toVector, Seq(), rng )
  })

  def KnuthShuffleInner[T](j: Int, X: ArrayBuffer[T], rng: RNG): (Seq[T], RNG) = { // Algorithm P Shuffling by Knuth
    val (u,y) = double.run(rng)
    val k = Math.floor(j*u).toInt + 1
    // shift by 1 indices from Knuth to be 0-based.
    val tmp = X(k-1)
    X(k-1) = X(j-1)
    X(j-1) = tmp
    // no longer shift by 1. Resume Knuth's indices.
    if (j ==1 ) (X, y)
    else KnuthShuffleInner(j-1, X, y)
  }

  def KnuthShuffle[T]( s: ArrayBuffer[T]): Rand[Seq[T]] = State( rng => { // Algorithm P Shuffling by Knuth
    KnuthShuffleInner(s.size, s, rng) // shuffle from last card to first in N steps.
  })

  protected def randomWithRepeats(s: Seq[Int], k: Int)(rng: RNG): (List[Int], RNG) = {
    val (ints, r) = sequence(List.fill(k)(nonNegativeLessThan(s.size))).run(rng)
    (ints.map(i => s(i)), r)
  }

  case class SelectorState(chosen: Seq[Int], available: Set[Int])

  def update = (i: Int) => (s: SelectorState) =>
    if (s.chosen.contains(i)) s  // failed to improve
    else SelectorState(s.chosen ++ Seq(i), s.available - i) // succeeded

  def execute(inputs: List[Int]): State[SelectorState, (Seq[Int], Set[Int])] = for {
    _ <- State.rawSequence(inputs map (modify[SelectorState] _ compose update))
    s <- get
  } yield (s.chosen, s.available)

  // no repeats in sample. Assumes input sequence has no duplicates.
  // Consequence of not reversing is that using the same seed on same inputs for k1 < k2 will not necessarily have the same prefix chain of k1
  // for the two calls of k1 and k2. It is still pure function, but subtle in behaviour.
  def sampleElements(s: Seq[Int], k: Int): Rand[Seq[Int]] = {
    // cannot do @tailrec simply because of divide and conquer into 2 parts.
    // Conceivably, go could use par; would require experiments to validate if thread overhead justifies it.
    def go(sel: SelectorState, k: Int, r: RNG): (Seq[Int], RNG) = {
      if (k < sel.available.size / 2 || k <= 8) { // experiments showed 8 to 32 not too bad. Would require deeper analysis.
        val (ints, rr) = randomWithRepeats(sel.available.toSeq, k)(r) // expected that there are repeats but when k is small they can be unique (in particular k =1)
        val intsSetSize = ints.toSet.size
        val transformer = execute(ints)
        transformer.run(sel) match {
          // for small k this may match
          case ((newSeq, newSet), newSelector) if intsSetSize == k =>
            (newSelector.chosen, rr)
          case ((newSeq, newSet), newSelector) =>
            go(newSelector, k - intsSetSize, rr) // this should improve each time because we're forced to select something new
        }
      }
      else {
        val (seq1, seq2) = sel.available.splitAt(sel.available.size/2)
        val x1 = go(SelectorState(sel.chosen, seq1), k/2, r)
        val x2 = go(SelectorState(sel.chosen, seq2), k - k/2, x1._2)
        (x1._1 ++ x2._1, x2._2)
      }
    }
    State(rng => go (SelectorState(Seq(), s.toSet), Math.min(k, s.size), rng))
  }

  def shuffle(s: Seq[Int]): Rand[Seq[Int]] = collectSample(s, s.size)

  // no repeats in sample
  def collectSample[T](s: Seq[T], k: Int): Rand[Seq[T]] = State(rng =>  {
    val (seq, r) = sampleElements(s.indices, k).run(rng)
    (seq.map(i => s(i)), r)
  })

}

object State {
type Rand[A] = State[RNG, A]

def unit[S, A](a: A): State[S, A] =
State(s => (a, s))


// This implementation uses a loop internally and is the same recursion
// pattern as a left fold. It is quite common with left folds to build
// up a list in reverse order, then reverse it at the end.
//
// Here, we defined intermediate rawSequence not to reverse for clients interested in lower latency and for which
// the operations are considered left and right associative (e.g. selecting k elements randomly, we don't care about the reverse
// being "better" or "worse" than its "conjugate".
def rawSequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
@tailrec
def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
 actions match {
   case Nil => (acc,s)
   case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
 }
State((s: S) => go(s,sas,List()))
}
// the preferred interface by far as it preserves associativity rules
def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
rawSequence(sas).map( _.reverse)
}


def modify[S](f: S => S): State[S, Unit] = for {
s <- get // Gets the current state and assigns it to `s`.
_ <- set(f(s)) // Sets the new state to `f` applied to `s`.
} yield ()

def get[S]: State[S, S] = State(s => (s, s))

def set[S](s: S): State[S, Unit] = State(_ => ((), s))
}
