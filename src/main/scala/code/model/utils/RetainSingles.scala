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

  def retainSingles[A: KeyHolder](as: Seq[A])(implicit ev: KeyHolder[A]): Iterable[A] = {
    val splits = as.groupBy(ev.getKey)
    splits.map(_._2.head) // we don't care if there is more than one per group (duplicates) as this is expected as normal.
  }
}
