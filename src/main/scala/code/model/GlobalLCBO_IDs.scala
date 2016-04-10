package code.model


import scala.language.implicitConversions
/**
  * Created by philippederome on 2016-04-04.
  *
  * @see Implicit Class http://docs.scala-lang.org/overviews/core/implicit-classes.html
  */

object GlobalLCBO_IDs {
  // distinguish two usages of Long types to avoid confusing our primary key with the LCBO's primary key.
  implicit class LCBO_ID(val underlying: Long) extends AnyVal {
    override def toString = underlying.toString
  }
  implicit def externalIDtoLong(id: LCBO_ID): Long = id.underlying  // implicit view

  implicit class P_KEY(val underlying: Long) extends AnyVal {
    override def toString = underlying.toString
  }
  implicit def internalIDtoLong(id: P_KEY): Long = id.underlying // implicit view
}