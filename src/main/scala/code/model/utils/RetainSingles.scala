package code.model.utils

import net.liftweb.common.Loggable
import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-05-12.
  */
object RetainSingles extends Loggable {
  // Does not preserve order of incoming sequence. If needed, use something else. Meaning we don't care which element match if multiple ones match same key
  // Why pick up first from incoming sequence values?
  def asMap[K, V](values: Iterable[V], toK: V => K): Map[K, V] =
    values.groupBy(toK).map { case(k, vs) => k -> vs.head }

  // Use of "pimp/enrich my library" pattern. Does not preserve order of incoming sequence. If needed, use something else.
  def retainSinglesImpure[A <: KeyHolder](as: Seq[A]): Iterable[A] = {
    val splits = as.groupBy(_.getKey)
    splits.map(_._2.head) // we don't care if there is more than one per group (duplicates) as this is expected as normal.
  }

  // this could be generalized to a full-fledge error reporter as opposed to using logger.
  def onFailureDefault[A <: KeyHolder](items: Iterable[A]): Unit =
    items.foreach(x => logger.warn(s"discarded key:${x.getKey} item:$x"))

  // @see http://www.scala-notes.org/2010/06/avoid-structural-types-when-pimping-libraries/
  class ResultImpure[A <: KeyHolder](as: Seq[A])(implicit ev: Iterable[A] => Unit = onFailureDefault _) {
    def retainSinglesImpure: Iterable[A] = RetainSingles.retainSinglesImpure(as)
  }
  // Use of "pimp/enrich my library" pattern (removeDupes).
  implicit def implicitSeqToImpure[A <: KeyHolder](as: Seq[A])(implicit ev: Iterable[A] => Unit = onFailureDefault _): ResultImpure[A] =
    new ResultImpure(as)(ev)

  class ResultPure[A <: KeyHolder](as: Seq[A]) {
    def retainSinglesPure: Iterable[A] = RetainSingles.retainSinglesImpure(as)
  }
  implicit def implicitSeqToPure[A <: KeyHolder](as: Seq[A]): ResultPure[A] = new ResultPure(as)
}
