package code.model

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-03-25.
  */
trait IProduct {
  def lcboId: Long

  def pKey: Long

  def Name: String
  def primaryCategory: String
  def isDiscontinued: Boolean
  def totalPackageUnits: Int
  def imageThumbUrl: String
  def Package: String

  // Change unit of currency from cents to dollars and Int to String
  def price: String
  def isDirty(p: IProduct): Boolean
  def streamAttributes: IndexedSeq[Attribute]

}

