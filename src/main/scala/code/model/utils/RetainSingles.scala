package code.model.utils

import net.liftweb.common.Loggable
import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-05-12.
  */
object RetainSingles extends Loggable {
  def asMap[K, T](items: Iterable[T], toK: T => K): Map[K,T] = {
    var keys = Set.empty[K]
    items.foldLeft(Map.empty[K, T]) { (acc, x) =>
      val k = toK(x)
      if (keys(k)) acc
      else {
        keys += k
        acc + (k -> x)
      }
    }
  }

  def generalRemoveDupes[T <: KeyHolder](items: Iterable[T])(onFailure: Iterable[T] => Unit): Iterable[T] = {
      var inKeys = Set.empty[String]
      var retained = Seq.empty[T]
      var discarded = Seq.empty[T]
      items.foreach { x: T =>
        val k = x.getKey
        if (inKeys(k)) discarded ++= Seq(x)
        else {
          inKeys += k
          retained ++= Seq(x)
        }
      }
      onFailure(discarded)
      retained
    }

  // this could be generalized to a full-fledge error reporter as opposed to using logger.
  def logDiscarded[T <: KeyHolder](items: Iterable[T]): Unit =
    items.foreach(x => logger.warn(s"discarded key:${x.getKey} item:$x"))

  // default is noisy.
  def removeDupes[T <: KeyHolder](items: Iterable[T]): Iterable[T] =
    generalRemoveDupes(items)(logDiscarded)

  // Minor use of "pimp/enrich my library" pattern (removeDupeds). Uses reflexion mind you.
  implicit def iterToSyntax[A <: KeyHolder](iter: Iterable[A]) = new {
    def removeDupeds = removeDupes(iter)
  }

  val noop = (a: Any) => ()
  def removeDupesQuietly[T <: KeyHolder](items: Iterable[T]): Iterable[T] =
    generalRemoveDupes(items){ noop }
}
