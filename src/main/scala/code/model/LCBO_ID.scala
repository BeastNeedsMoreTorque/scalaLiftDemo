package code.model

import scala.language.implicitConversions
/**
  * Created by philippederome on 2016-04-04.
  */

case class LCBO_ID(x: Long) {
  override def toString = x.toString // don't want to send into DOM some LCBO_ID wrapper around
}

object LCBO_ID {
  implicit def LCBO_IDtoLong(id: LCBO_ID): Long = id.x
}

case class P_KEY(x: Long) {
  override def toString = x.toString // don't want to send into DOM some P_KEY wrapper around x
}

object P_KEY {
  implicit def P_KEYtoLong(id: P_KEY): Long = id.x
}