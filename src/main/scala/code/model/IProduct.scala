package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-03-25.
  */
trait IProduct extends Equals with KeyKeeper {
  def Name: String
  def primaryCategory: String
  def isDiscontinued: Boolean
  def imageThumbUrl: String

  // Change unit of currency from cents to dollars and Int to String
  def price: String

  // @see Scala in Depth
  override def canEqual(other: Any): Boolean =
    other.isInstanceOf[IProduct]

  override def hashCode: Int = (Name + price).## // if the names are the same, they're probably the same products, but price is a bit volatile too.

  override def equals(other: Any): Boolean =
    other match {
      case that: IProduct =>
        if (this eq that) true
        else {
          that.## == this.## &&
          that.canEqual(this) &&
          ( Name == that.Name &&
            primaryCategory == that.primaryCategory &&
            isDiscontinued == that.isDiscontinued &&
            imageThumbUrl == that.imageThumbUrl &&
            price == that.price )
        }
      case _ => false
    }

  def streamAttributes: IndexedSeq[Attribute]

}

