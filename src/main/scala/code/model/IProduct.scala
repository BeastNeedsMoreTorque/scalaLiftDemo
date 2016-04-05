package code.model

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

  override def equals(o: Any) = o match {
    case that: IProduct => price == that.price &&
      imageThumbUrl == that.imageThumbUrl
    case _ => false
  }
  val dirtyPredicate: (IProduct, IProduct) => Boolean = {(x, y)=> !x.equals(y)}
  def getCachedItem: (IProduct) => Option[IProduct]
  def streamAttributes: IndexedSeq[Attribute]

}

