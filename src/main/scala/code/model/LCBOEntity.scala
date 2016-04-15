package code.model

import scala.collection.mutable.ArrayBuffer
import scala.collection.IndexedSeq
import code.model.pageFetcher.{LCBOPageFetcher, LCBOPageFetcherComponentImpl}

import net.liftweb.json.JsonAST.{JField, JInt}
/**
  * Created by philippederome on 2016-04-10. Highest level trait to share between Product and Store that have much logic in common.
  */
trait LCBOEntity[T <: LCBOEntity[T]] extends Persistable[T]
  with CreatedUpdated[T] with LCBOPageFetcher with LCBOPageFetcherComponentImpl with ItemStateGrouper with ErrorReporter {
  self: T =>

  // Some LCBO entities require to back patch JSon read in "id" as a separate column in Record (lcbo_id). They do so with the logic below (idFix = transform).
  // In other words, JSON comes in as id=123 and we need to store that to table.column[lcbo_id]. The crux of problem is Lift Record wanting to use Fields
  // that have a functional read-only interface while accepting to do sets on the columns and that clashes with underlying Squeryl ORM library that has defined
  // id as a def (a true read-only item). And this id thingie is required for the whole MainSchema to work with the ORM relationships in memory.
  val extract: JSitemsExtractor[T] = { json =>
    val idFix = json transform {
      case JField("id", JInt(n)) => JField("lcbo_id", JInt(n)) // see above paragraph text for justification.
    }
    val nodes = idFix.children
    nodes.foldLeft(ArrayBuffer[T]()) {
      (recsBuffer, node) =>
        for (rec <- meta.fromJValue(node)) { // a lcbo_id can be set here, but not an id (it's kind of "reserved" word by Squeryl while this call is Lift Record).
          recsBuffer.append(rec)
        }
        recsBuffer
    }.toIndexedSeq
  }

  // type parameter I should be an interface of T, so that getCachedItem can return an interface rather than a concrete class, and it should not return just anything.
  // Some LCBO entities also have a similar pattern of identifying new info from LCBO (items being provided from a query), reconciling/interpreting as new or dirty (or clean/unchanged)
  // and then make sure first DB is brought up to date with that info and synchronously the cache memory as well.
  final def synchDirtyAndNewItems[I >: T](items: IndexedSeq[T], get: (I) => Option[I], p: (I, T) => Boolean): Unit = {
    val (dirtyItems, newItems) = itemsByState[I, T](items, get, p)
    updateAndInsert(dirtyItems, newItems) // updates DB AND cache.
  }
}
