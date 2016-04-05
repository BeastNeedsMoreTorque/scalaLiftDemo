package code.model

import scala.language.implicitConversions
/**
  * Created by philippederome on 2016-04-04.
  */

case class LCBO_ID(x: Long) { override def toString = x.toString} // don't want to send into DOM some LCBO_ID wrapper around x
case class P_KEY(x: Long) { override def toString = x.toString}  // don't want to send into DOM some P_KEY wrapper around x

object GlobalIds {
  def LCBO_ID_to_Long(L: LCBO_ID) = L.x // to facilitate implicit conversions to Long, yet prevent confusion in mixing up pKey and lcboId usage
  def P_KEY_to_Long(k: P_KEY): Long = k.x // to facilitate implicit conversions to Long, yet prevent confusion in mixing up pKey and lcboId usage
}
