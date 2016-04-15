package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-04-01.
  */
case class DirtyAndNewSequences[T](dirtys: IndexedSeq[T], news: IndexedSeq[T]) {}

trait ItemStateGrouper {
  // partition items into 3 lists, clean (no change), new (to insert) and dirty (to update), using neat groupBy, predicated on inputs being indexedseq for efficiency of groupby.
  // return to user only new and dirty since there is not much to do with clean, at least for now.
  // We want I to be an interface of T when using getCachedItem.
  def itemsByState[I, T <: I](items: IndexedSeq[T],
                              get: I => Option[I],
                              isDirty: (I, T) => Boolean): DirtyAndNewSequences[T] = {
    val x = items.groupBy {
      latest => (get(latest), latest) match {
        case (None, _) => EntityRecordState.New
        case (Some(retrieved), latest) if isDirty(retrieved, latest) => EntityRecordState.Dirty
        case (_, _) => EntityRecordState.Clean
      }
    }
    val dirtyItems = x.getOrElse(EntityRecordState.Dirty, IndexedSeq[T]())
    val newItems = x.getOrElse(EntityRecordState.New, IndexedSeq[T]())
    DirtyAndNewSequences(dirtyItems, newItems)
  }
}