package code.model.utils

//Copyright (c) 2012, Manning Publications, Co.

//Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

//The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

// Edited and modified by Phil Derome, abiding by above copyright from Manning Publications, Co. Modified Software is provided "AS IS" as per above as well.

// Apparently, this is in line of thought of Scalaz (authors probably contributed to Scalaz).

import State._

import scala.annotation.tailrec

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

  protected def randomWithRepeats(s: Seq[Int], k: Int)(rng: RNG): (List[Int], RNG) = {
    val (ints, r) = sequence(List.fill(k)(nonNegativeLessThan(s.size))).run(rng)
    (ints.map(i => s(i)), r)
  }

  case class SelectorState(chosen: Seq[Int], available: Set[Int])

  def update = (i: Int) => (s: SelectorState) =>
    if (s.chosen.exists(_ == i)) s  // failed to improve
    else SelectorState(s.chosen ++ Seq(i), s.available - i) // succeeded

  def execute(inputs: List[Int]): State[SelectorState, (Seq[Int], Set[Int])] = for {
    _ <- State.rawSequence(inputs map (modify[SelectorState] _ compose update))
    s <- get
  } yield (s.chosen, s.available)

  // no repeats in sample
  // Consequence of not reversing is that using the same seed on same inputs for k1 < k2 will not necessarily have the same prefix chain of k1
  // for the two calls of k1 and k2. It is still pure function, but subtle in behaviour.
  def sampleElements(s: Seq[Int], k: Int): Rand[Seq[Int]] = {
    @tailrec
    def go(sel: SelectorState, in_K: Int, r: RNG): (Seq[Int], RNG) = {
      val k = Math.min(in_K, s.size) // effective k
      val (ints, rr) = randomWithRepeats(sel.available.toSeq, k)(r) // expected that there are repeats but when k is small they can be unique (in particular k =1)
      val intsSetSize = ints.toSet.size
      val transformer = execute(ints)
      transformer.run(sel) match {
        // with great luck or very small k they'd be distinct
        case ((newSeq, newSet), newSelector) if intsSetSize == k =>
          (newSelector.chosen, rr)
        case ((newSeq, newSet), newSelector) =>
          go(newSelector, k - intsSetSize,rr) // this should improve each time because we're forced to select something new
      }
    }
    State(rng => go (SelectorState(Seq(), s.toSet), k, rng))
  }

  def shuffle(s: Seq[Int]): Rand[Seq[Int]] = sampleElements(s, s.size)

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
