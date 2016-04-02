package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-04-01.
  */

trait LcboItem[T, I] {

  type EnumerationValueType = Enumeration#Value
  // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
  def itemsByState(items: IndexedSeq[T], getCachedItem: (T) => Option[I], dirtyPred: (I, T) => Boolean): Map[EnumerationValueType, IndexedSeq[T]] = {
    items.groupBy {
      src => (getCachedItem(src), src) match {
        case (None, _) => EntityRecordState.New
        case (Some(item), srcItem) if dirtyPred(item, srcItem) => EntityRecordState.Dirty
        case (_, _) => EntityRecordState.Clean
      }
    }
  }
}