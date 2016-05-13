package code.model.utils

import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-05-12.
  */
object RetainSingles {
  def asMap[K, T](items: Iterable[T], toK: T => K): Map[K,T] =
     items.foldLeft(Map.empty[K,T]){(acc, x) =>
      val k = toK(x)
      if (acc.keySet(k)) acc else acc + (k -> x)
    }

  def filter[K, T](items: Iterable[T], toK: T => K): Iterable[T] =
    asMap(items, toK).values

}
