package code.model

import scala.collection.mutable.ArrayBuffer
import scala.collection.IndexedSeq

import code.model.GlobalLCBO_IDs.LCBO_ID
import code.model.pageFetcher.LCBOPageFetcher

/**
  * Created by philippederome on 2016-04-10. Highest level trait to share between Product and Store that have much logic in common.
  */
trait LCBOEntity[T <: LCBOEntity[T]] extends LCBOPageFetcher with Persistable[T]
  with CreatedUpdated[T] with ItemStateGrouper with propsSeqReader with ErrorReporter {
  self: T=>

  def setLcboId(id: LCBO_ID): Unit

  // Some LCBO entities require to back patch JS read in "id" as a separate column in Record. They do so in with the same logic below.
  val LcboExtract: JSitemsExtractor[T] =  { nodes =>
    nodes.foldLeft(ArrayBuffer[T]()) {
      (recsBuffer, node) =>
        for (key <- (node \ "id").extractOpt[Long];
             rec <- meta.fromJValue(node)) {
          rec.setLcboId(LCBO_ID(key)) //hack. Record is forced to use "id" as read-only def, which means we cannot extract it direct... Because of PK considerations at Squeryl (KeyedEntity has def id: K, which is read-only).
          recsBuffer.append(rec)
        }
        recsBuffer
    }.toIndexedSeq
  }

  // type parameter I should be an interface of T, so that getCachedItem can return an interface rather than a concrete class, and it should not return just anything.
  // Some LCBO entities also have a similar pattern of identifying new info from LCBO (items being provided from a query), reconciling/interpreting as new or dirty (or clean/unchanged)
  // and then make sure first DB is brought up to date with that info and synchronously the cache memory as well.
  def synchDirtyAndNewItems[I >: T](items: IndexedSeq[T], getCachedItem: (I) => Option[I], dirtyPred: (I, T) => Boolean): Unit = {
    val (dirtyItems, newItems) = itemsByState[I, T](items, getCachedItem, dirtyPred)
    updateAndInsert(dirtyItems, newItems) // updates DB AND cache.
  }
}
