package code.model

import scala.language.implicitConversions
/**
  * Created by philippederome on 2016-04-04.
  *
  * @see Implicit Class http://docs.scala-lang.org/overviews/core/implicit-classes.html
  * @see http://blog.scalac.io/2016/05/26/simple-types-in-play.html
  * @see http://docs.scala-lang.org/overviews/core/value-classes.html
  *      Combine to allocation-free extension methods, meaning LCBO_ID and P_KEY are often using the same overhead as Long
  *      rather than that of a full object with several exceptions (SIP-13 or http://jorgeortiz85.github.io/ImplicitClassSIP.xhtml)
  */

object GlobalLCBO_IDs {
  // tag definition:
  type Tagged[A, T] = {type Tag = T; type Self = A}
  type @@[T, Tag] = Tagged[T, Tag]

  trait LcboKeyTag
  type LCBO_KEY = Long @@ LcboKeyTag

  trait PKEYTag
  type P_KEY = Long @@ PKEYTag

  implicit class TaggedLong(val underlying: Long) extends AnyVal {
    def LcboKeyID: LCBO_KEY = underlying.asInstanceOf[LCBO_KEY]
    def PKeyID: P_KEY = underlying.asInstanceOf[P_KEY]
  }

  implicit def externalIDtoLong(id: LCBO_KEY): Long = id.asInstanceOf[Long] // implicit view
  implicit def internalIDtoLong(id: P_KEY): Long = id.asInstanceOf[Long] // implicit view
}
