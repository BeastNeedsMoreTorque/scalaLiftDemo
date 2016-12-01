package code.model

import code.model.GlobalLCBO_IDs.{LCBO_KEY, P_KEY}
import scala.language.implicitConversions

trait KeyKeeper[A] {
  def lcboKey(f: A): LCBO_KEY
  def pKey(f: A): P_KEY
}

object KeyKeeper {
  def getKeyPair[A](lc: A => LCBO_KEY, pk: A => P_KEY): KeyKeeper[A] = new KeyKeeper[A] {
    def lcboKey(a: A): LCBO_KEY = lc(a)
    def pKey(a: A): P_KEY = pk(a)
  }
  implicit val productKeyPairKeeper: KeyKeeper[IProduct] = KeyKeeper.getKeyPair({ prod: IProduct => prod.lcboKey}, {prod: IProduct => prod.pKey })
}

case class KeyKeeperVals(lcboKey: LCBO_KEY, pKey: P_KEY)

object KeyKeeperVals {
  implicit def toKeyKeeperVals[A](a: A)(implicit ev: KeyKeeper[A]): KeyKeeperVals = KeyKeeperVals(ev.lcboKey(a), ev.pKey(a))
}
