package code.model

import code.model.GlobalLCBO_IDs.{LCBO_KEY, P_KEY}
import code.model.ShowKeyPair.ShowKeyPairVals

// patterned after Show type class, it is meant to show a pair of "keys" from A, one external one internal
trait ShowKeyPair[External, Internal, A] {
  def showE(a: A): External
  def showI(a: A): Internal
  def showKeyPairVals(a: A) = ShowKeyPairVals(showE(a), showI(a))
}

object ShowKeyPair {
  case class ShowKeyPairVals[E, I](eKey: E, iKey: I)
  implicit def productKeyPairKeeper: ShowKeyPair[LCBO_KEY, P_KEY, IProduct] = new ShowKeyPair[LCBO_KEY, P_KEY, IProduct] {
    def showE(prod: IProduct) = prod.lcboKey
    def showI(prod: IProduct) = prod.pKey
  }
  implicit class ShowKeyPairOps[E,I,A](a: A)(implicit ev: ShowKeyPair[E, I, A]) {
    def showKeyPairVals: ShowKeyPairVals[E, I] = ev.showKeyPairVals(a)
  }
}
