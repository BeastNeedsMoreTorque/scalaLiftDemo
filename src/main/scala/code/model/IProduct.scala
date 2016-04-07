package code.model

import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-03-25.
  */
trait IProduct  {
  def lcboId: LCBO_ID

  def pKey: P_KEY

  def Name: String
  def primaryCategory: String
  def isDiscontinued: Boolean
  def totalPackageUnits: Int
  def imageThumbUrl: String
  def Package: String

  // Change unit of currency from cents to dollars and Int to String
  def price: String

  // doing a proper equals entails hashing too, so skip that, plus we want similar enough more than equals in reality
  def dirty(o: Any) = o match {
    case that: IProduct => price != that.price ||
      imageThumbUrl != that.imageThumbUrl
    case _ => true
  }
  val dirtyPredicate: (IProduct, IProduct) => Boolean = {(x, y)=> x.dirty(y)}
  def getCachedItem: (IProduct) => Option[IProduct]
  def streamAttributes: IndexedSeq[Attribute]

}

