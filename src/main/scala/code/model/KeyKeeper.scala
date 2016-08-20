package code.model

import code.model.GlobalLCBO_IDs.{LCBO_KEY, P_KEY}
import code.model.utils.KeyHolder
import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-04-12.
  */
trait KeyKeeper extends KeyHolder {
  override def getKey: String = lcboKey.toString
  def lcboKey: LCBO_KEY
  def pKey: P_KEY
}

case class KeyKeeperVals(lcboKey: LCBO_KEY, pKey: P_KEY)

object KeyKeeperVals {
  implicit def toKeyKeeperVals(k: KeyKeeper): KeyKeeperVals = KeyKeeperVals(k.lcboKey, k.pKey)
}
