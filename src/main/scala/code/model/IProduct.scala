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
  def streamAttributes: IndexedSeq[Attribute]
}

