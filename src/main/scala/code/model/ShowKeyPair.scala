package code.model

import code.model.GlobalLCBO_IDs.{LCBO_KEY, P_KEY}

// patterned after Show type class, it is meant to show a pair of "keys" from A, one external (E) one internal (I)
trait ShowKeyPair[A, E, I] { // E: external, I: Internal
  def showE(f: A): E
  def showI(f: A): I
}

object ShowKeyPair {
  def apply[A, E, I](f: A => E, g: A => I): ShowKeyPair[A, E, I] = new ShowKeyPair[A, E, I] {
    def showE(a: A): E = f(a)
    def showI(a: A): I = g(a)
  }
  implicit def productKeyPairKeeper: ShowKeyPair[IProduct, LCBO_KEY, P_KEY] = ShowKeyPair(prod => prod.lcboKey, prod => prod.pKey)
}

case class ShowKeyPairVals(lcboKey: LCBO_KEY, pKey: P_KEY)

object ShowKeyPairVals {
  implicit def toShowKeyPairVals[A](a: A)(implicit ev: ShowKeyPair[A, LCBO_KEY, P_KEY]): ShowKeyPairVals =
    ShowKeyPairVals(ev.showE(a), ev.showI(a))
}
