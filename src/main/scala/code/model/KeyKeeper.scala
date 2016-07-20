package code.model

import code.model.GlobalLCBO_IDs.{LCBO_ID, P_KEY}
import code.model.utils.KeyHolder

import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-04-12.
  */
trait KeyKeeper extends KeyHolder {
  override def getKey: String = lcboId.toString
  def lcboId: LCBO_ID
  def pKey: P_KEY
}

case class KeyKeeperVals(lcboId: LCBO_ID, pKey: P_KEY){}

object KeyKeeperVals {
  implicit def toKeyKeeperVals(k: KeyKeeper): KeyKeeperVals = KeyKeeperVals(k.lcboId, k.pKey)
}
