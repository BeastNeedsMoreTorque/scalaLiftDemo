package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-04-01.
  */
case class UpdateAndInsertSequences[T](updates: IndexedSeq[T], inserts: IndexedSeq[T])

trait ItemStateGrouper {
  // to denote whether an abstract item T (for example Application Lift Record) requires to be inserted (New), updated (Dirty), or is good as is (Clean)
  // Client of trait makes such determination. We return an ordered pair of the dirty ones and then the new ones.

  sealed trait EntityRecordState
  case object New extends EntityRecordState
  case object Dirty extends EntityRecordState
  case object Clean extends EntityRecordState

  // We want I to be an interface of T when using get/isDirty as get usage could be more abstract than type T at client side (possibly retrieving from cache).
  // Returned sequences require to be concrete because that is how our ORM interface is like.
  // In places like this, (implicit evI: TypeTag[I], evT: TypeTag[T])
  // could be used all the way to concrete class users to identify possible info on type parameter
  def itemsByState[I, T <: I](items: IndexedSeq[T],
                              cached: I => Option[I]): UpdateAndInsertSequences[T] = {
    def empty = IndexedSeq.empty[T]

    val x = items.groupBy {
      current => (cached(current), current) match {
        case (None, _) => New
        case (Some(retrieved), curr) if retrieved == curr => Clean
        case _ => Dirty
      }
    }
    UpdateAndInsertSequences(x.getOrElse(Dirty, empty), x.getOrElse(New, empty))
  }
}
