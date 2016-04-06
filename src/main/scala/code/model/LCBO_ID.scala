package code.model

import scala.language.implicitConversions
/**
  * Created by philippederome on 2016-04-04.
  *
  * @see Value Class, Extension Methods, Correctness (Meter example fitting what we do ) http://docs.scala-lang.org/overviews/core/value-classes.html
  *      commentary effectively states that JVM limitations force allocations of value classes, most likely when using containers such as they keys
  *      of my TrieMaps. But, it points the way for more efficiency down the line in the future.
  */

object GlobalLCBO_IDs {

  // distinguish two usages of Long types to avoid confusing our primary key with the LCBO's primary key.
  implicit class LCBO_ID(val underlying: Long) extends AnyVal {
    // original toString won't do.
    override def toString = underlying.toString // don't want to send into DOM some TYPE wrapper around the number
  }

  object LCBO_ID {
    implicit def toLong(id: LCBO_ID): Long = id.underlying
  }

  implicit class P_KEY(val underlying: Long) extends AnyVal {
    override def toString = underlying.toString // don't want to send into DOM some TYPE wrapper around the number
  }

  object P_KEY {
    implicit def toLong(id: P_KEY): Long = id.underlying
  }

}