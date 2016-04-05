package code.model

/**
  * Created by philippederome on 2016-04-04.
  */

case class LCBO_ID(x: Long) { override def toString = x.toString} // don't want to send into DOM some LCBO_ID wrapper around x
case class P_KEY(x: Long) { override def toString = x.toString}  // don't want to send into DOM some P_KEY wrapper around x
