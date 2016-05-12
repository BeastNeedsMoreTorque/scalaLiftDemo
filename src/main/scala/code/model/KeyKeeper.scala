package code.model

import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-04-12.
  */
trait KeyKeeper {
  def lcboId: LCBO_ID
  def pKey: P_KEY
}


case class KeyKeeperVals(lcboId: LCBO_ID, pKey: P_KEY){}

object KeyKeeperVals {
  implicit def toKeyKeeperVals(k: KeyKeeper) = KeyKeeperVals(k.lcboId, k.pKey)
}