package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-04-01.
  */
trait ItemStateGrouper[T, I] { // I could be an interface of T but does not have to be.

  type EnumerationValueType = Enumeration#Value
  // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy, predicated on inputs being indexedseq for efficiency of groupby.
  def itemsByState(items: IndexedSeq[T], getCachedItem: (T) => Option[I], dirtyPred: (I, T) => Boolean): Map[EnumerationValueType, IndexedSeq[T]] = {
    items.groupBy {
      latest => (getCachedItem(latest), latest) match {
        case (None, _) => EntityRecordState.New
        case (Some(cached), latest) if dirtyPred(cached, latest) => EntityRecordState.Dirty
        case (_, _) => EntityRecordState.Clean
      }
    }
  }
}