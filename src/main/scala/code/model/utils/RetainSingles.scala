package code.model.utils

import net.liftweb.common.Loggable
import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-05-12.
  */
object RetainSingles extends Loggable {
  def asMap[K, V](values: Iterable[V], toK: V => K): Map[K,V] = {
    case class asMapState[K,V](s: Set[K], m: Map[K,V])
    values.foldLeft(asMapState(Set.empty[K], Map.empty[K, V])) { (acc, x) =>
      val k = toK(x)
      if (acc.s(k)) acc
      else asMapState(acc.s + k, acc.m + (k -> x))
    }.m
  }

  def generalRemoveDupes[T <: KeyHolder](items: Seq[T])(onFailure: Seq[T] => Unit): Seq[T] = {
    case class RemoveDupesState[T](keys: Set[String], discarded: IndexedSeq[T], retained: IndexedSeq[T])
    val pair = items.reverse.foldLeft(RemoveDupesState(Set.empty[String], IndexedSeq.empty[T], IndexedSeq.empty[T])) { (acc, item) =>
      val k = item.getKey
      if (acc.keys(k)) RemoveDupesState(keys=acc.keys, discarded= item +: acc.discarded, retained=acc.retained)
      else RemoveDupesState(keys=acc.keys + k, discarded=acc.discarded, retained= item +: acc.retained)
    }
    onFailure(pair.discarded)
    pair.retained
  }

  // this could be generalized to a full-fledge error reporter as opposed to using logger.
  def logDiscarded[T <: KeyHolder](items: Iterable[T]): Unit =
    items.foreach(x => logger.warn(s"discarded key:${x.getKey} item:$x"))

  // default is noisy.
  def removeDupes[T <: KeyHolder](items: Seq[T]): Seq[T] =
    generalRemoveDupes(items)(logDiscarded)

  // @see http://www.scala-notes.org/2010/06/avoid-structural-types-when-pimping-libraries/
  class Result[A <: KeyHolder](as: Seq[A]) {
    def removeDupeds = removeDupes(as)
  }

  // Use of "pimp/enrich my library" pattern (removeDupeds).
  implicit def implicitSeqToSyntax[A <: KeyHolder](as: Seq[A]) = new Result(as)

  val noop = (a: Any) => ()
  def removeDupesQuietly[T <: KeyHolder](items: Seq[T]): Seq[T] =
    generalRemoveDupes(items){ noop }
}
