package code.model

import code.model.GlobalLCBO_IDs.{LCBO_KEY, P_KEY}
import code.model.utils.KeyHolder
import scala.language.implicitConversions

/**
  * Created by philippederome on 2016-04-12.
  */
trait KeyKeeper[A] extends KeyHolder[A] {
  def lcboKey(f: A): LCBO_KEY
  def pKey(f: A): P_KEY
  override def getKey(f: A): String = lcboKey(f).toString
}

object KeyKeeper {
  implicit val productKeeper = new KeyKeeper[IProduct] {
    def lcboKey(f: IProduct): LCBO_KEY = f.lcboKey
    def pKey(f: IProduct): P_KEY = f.pKey
  }

  implicit val storeKeeper = new KeyKeeper[IStore] {
    def lcboKey(f: IStore): LCBO_KEY = f.lcboKey
    def pKey(f: IStore): P_KEY = f.pKey
  }
}

case class KeyKeeperVals(lcboKey: LCBO_KEY, pKey: P_KEY)

object KeyKeeperVals {
  implicit def toKeyKeeperVals[A](a: A)(implicit ev: KeyKeeper[A]): KeyKeeperVals = KeyKeeperVals(ev.lcboKey(a), ev.pKey(a))
}