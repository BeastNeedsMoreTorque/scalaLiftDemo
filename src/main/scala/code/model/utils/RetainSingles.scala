package code.model.utils

import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-05-12.
  */
object RetainSingles {
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

  def filter[K, T](items: Iterable[T], toK: T => K): Iterable[T] =
    asMap(items, toK).values // no attempt to apply "pimp/enrich my library" pattern or work with Builders here.

}
