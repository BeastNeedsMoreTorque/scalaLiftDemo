package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-04-01.
  */

trait LcboItem[T <: LcboItem[T, I] , I] {
  self: T =>

  type EnumerationValueType = Enumeration#Value

  def getItemByLcboId(id: Long): Option[I]
  def lcboId: Long

  // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy.
  def itemsByState(items: IndexedSeq[T]): Map[EnumerationValueType, IndexedSeq[T]] = {
    items.groupBy {
      p => (getItemByLcboId(p.lcboId), p) match {
        case (None, _) => EntityRecordState.New
        case (Some(item), srcItem) if !item.equals(srcItem) => EntityRecordState.Dirty
        case (_, _) => EntityRecordState.Clean
      }
    }
  }
}