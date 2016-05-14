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

  // Use of "pimp/enrich my library" pattern.
  def removeDupes[A <: KeyHolder](items: Seq[A])(onFailure: Seq[A] => Unit): Seq[A] = {
    case class RemoveDupesState[T](keys: Set[String], discarded: IndexedSeq[A], retained: IndexedSeq[A])
    val pair = items.reverse.foldLeft(RemoveDupesState(Set.empty[String], IndexedSeq.empty[A], IndexedSeq.empty[A])) { (acc, item) =>
      val k = item.getKey
      if (acc.keys(k)) RemoveDupesState(keys=acc.keys, discarded= item +: acc.discarded, retained=acc.retained)
      else RemoveDupesState(keys=acc.keys + k, discarded=acc.discarded, retained= item +: acc.retained)
    }
    onFailure(pair.discarded)
    pair.retained
  }

  // this could be generalized to a full-fledge error reporter as opposed to using logger.
  def onFailureDefault[A <: KeyHolder](items: Iterable[A]): Unit =
    items.foreach(x => logger.warn(s"discarded key:${x.getKey} item:$x"))

  // @see http://www.scala-notes.org/2010/06/avoid-structural-types-when-pimping-libraries/
  class Result[A <: KeyHolder](as: Seq[A])(implicit ev: Seq[A] => Unit = onFailureDefault _) {
    def removeDupes(implicit ev: Seq[A] => Unit) = RetainSingles.removeDupes(as)(ev)
  }

  // Use of "pimp/enrich my library" pattern (removeDupes).
  implicit def implicitSeqToSyntax[A <: KeyHolder](as: Seq[A])(implicit ev: Seq[A] => Unit = onFailureDefault _) = new Result(as)(ev)

  val noop = (a: Any) => ()
  def removeDupesQuietly[A <: KeyHolder](items: Seq[A]): Seq[A] =
    removeDupes(items){ noop }
}
