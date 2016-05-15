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
    values.groupBy(toK(_)).map { case(k, as) => k -> as.head }

  // Use of "pimp/enrich my library" pattern. Does not preserve order of incoming sequence. If needed, use something else.
  def removeDupes[A <: KeyHolder](as: Seq[A])(onFailure: Iterable[A] => Unit): Iterable[A] = {
    val splits = as.groupBy(_.getKey)
    onFailure(splits.flatMap(_._2.tail))
    splits.map(_._2.head)
  }

  // this could be generalized to a full-fledge error reporter as opposed to using logger.
  def onFailureDefault[A <: KeyHolder](items: Iterable[A]): Unit =
    items.foreach(x => logger.warn(s"discarded key:${x.getKey} item:$x"))

  // @see http://www.scala-notes.org/2010/06/avoid-structural-types-when-pimping-libraries/
  class Result[A <: KeyHolder](as: Seq[A])(implicit ev: Iterable[A] => Unit = onFailureDefault _) {
    def removeDupes(implicit ev: Iterable[A] => Unit) = RetainSingles.removeDupes(as)(ev)
  }

  // Use of "pimp/enrich my library" pattern (removeDupes).
  implicit def implicitSeqToSyntax[A <: KeyHolder](as: Seq[A])(implicit ev: Iterable[A] => Unit = onFailureDefault _): Result[A] =
    new Result(as)(ev)

  val noop = (a: Any) => ()
  def removeDupesQuietly[A <: KeyHolder](items: Seq[A]): Iterable[A] =
    removeDupes(items){ noop }
}
