package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-04-01.
  */

/**
  * Helper class that specifies which items represent required updates and required inserts
  */
case class UpdateAndInserts[T](updates: IndexedSeq[T], inserts: IndexedSeq[T])

/**
  * Single method trait that classifies items as Clean, Dirty, or New according to a comparison made with get.
  */
trait ItemStateGrouper {

  sealed trait EntityRecordState
  case object New extends EntityRecordState
  case object Dirty extends EntityRecordState
  case object Clean extends EntityRecordState

  /**
    * To denote whether an abstract item T (for example Application Lift Record) requires to be inserted (New), updated (Dirty), or is good as is (Clean)
    * Client of trait makes such determination. We return an ordered pair of the dirty ones and then the new ones.
    * We want I to be an interface of T when using get/isDirty as get usage could be more abstract than type T at client side (possibly retrieving from cache).
    * @param items input items that are assumed to have valid data that we need to classify for caching strategy
    * @param get user method to retrieve what is in memory for the given item based on item's key
    * @return pairs of sequences as updates and inserts within UpdateAndInserts
    */
  def itemsByState[I, T <: I](items: IndexedSeq[T],
                                     get: I => Option[I]): UpdateAndInserts[T] = {
    def empty = IndexedSeq.empty[T]

    val x = items.groupBy {
      current => (get(current), current) match {
        case (None, _) => New
        case (Some(retrieved), curr) if retrieved == curr => Clean
        case _ => Dirty
      }
    }
    UpdateAndInserts(x.getOrElse(Dirty, empty), x.getOrElse(New, empty))
  }
}
