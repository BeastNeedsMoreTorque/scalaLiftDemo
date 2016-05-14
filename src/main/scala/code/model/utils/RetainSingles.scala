package code.model.utils

import net.liftweb.common.Loggable

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

  // no attempt to apply "pimp/enrich my library" pattern or work with Builders here even if we didn't both with side effect of onFailure.
  def generalRemoveDupes[T <: KeyHolder](onFailure: Iterable[T] => Unit): Iterable[T] => Iterable[T] =
    items => {
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
  def removeDupes[T <: KeyHolder]: Iterable[T] => Iterable[T] =
    generalRemoveDupes(logDiscarded)

  val noop = (a: Any) => ()
  def removeDupesQuietly[T <: KeyHolder]: Iterable[T] => Iterable[T] =
    generalRemoveDupes{ noop }
}
