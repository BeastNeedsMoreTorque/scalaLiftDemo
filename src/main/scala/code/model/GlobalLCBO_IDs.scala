package code.model

import scala.language.implicitConversions
/**
  * Created by philippederome on 2016-04-04.
  *
  * @see Implicit Class http://docs.scala-lang.org/overviews/core/implicit-classes.html
  * @see http://docs.scala-lang.org/overviews/core/value-classes.html
  *      Combine to allocation-free extension methods, meaning LCBO_ID and P_KEY are often using the same overhead as Long
  *      rather than that of a full object with several exceptions (SIP-13 or http://jorgeortiz85.github.io/ImplicitClassSIP.xhtml)
  */

object GlobalLCBO_IDs {
  // distinguish two usages of Long types to avoid confusing our primary key with the LCBO's primary key.
  implicit class LCBO_ID(val underlying: Long) extends AnyVal {
    override def toString: String = underlying.toString
  }
  implicit def externalIDtoLong(id: LCBO_ID): Long = id.underlying  // implicit view

  implicit class P_KEY(val underlying: Long) extends AnyVal {
    override def toString: String = underlying.toString
  }
  implicit def internalIDtoLong(id: P_KEY): Long = id.underlying // implicit view
}
