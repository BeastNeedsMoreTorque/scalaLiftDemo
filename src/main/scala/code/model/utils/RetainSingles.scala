package code.model.utils

import code.model.{Inventory, KeyKeeper}

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

  def filter[K, T]( toK: T => K)(items: Iterable[T]): Iterable[T] =
    asMap(items, toK).values // no attempt to apply "pimp/enrich my library" pattern or work with Builders here.

  val removeDupesForInvs: Iterable[Inventory] => Iterable[Inventory] =
    RetainSingles.filter({inv: Inventory => (inv.productid, inv.storeid)})

  def removeDupesForLcboIds[T <: KeyKeeper]: Iterable[T] => Iterable[T] =
    RetainSingles.filter({k: T => k.lcboId})
}
