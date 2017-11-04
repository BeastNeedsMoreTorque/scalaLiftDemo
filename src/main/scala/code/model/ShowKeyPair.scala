package code.model

import code.model.GlobalLCBO_IDs.{LCBO_KEY, P_KEY}
import code.model.ShowKeyPair.ShowKeyPairVals

// patterned after Show type class, it is meant to show a pair of "keys" from A, one external one internal
trait ShowKeyPair[External, Internal, A] {
  def showE(a: A): External
  def showI(a: A): Internal
  def showKeyPairVals(a: A): ShowKeyPairVals[External, Internal] = ShowKeyPairVals(showE(a), showI(a))
}

object ShowKeyPair {

  case class ShowKeyPairVals[E, I](eKey: E, iKey: I)

  // remember those are defaulted provided implementations for IProduct and IStore, the caller can choose
  // any other implementation by selecting the proper evidence object implicitly in scope (in Haskell, there can only be one).
  implicit val productShowKeyPair = new ShowKeyPair[LCBO_KEY, P_KEY, IProduct] {
    def showE(prod: IProduct): LCBO_KEY = prod.lcboKey
    def showI(prod: IProduct): P_KEY = prod.pKey
  }

  implicit val storeShowKeyPair = new ShowKeyPair[LCBO_KEY, P_KEY, IStore] {
    def showE(store: IStore): LCBO_KEY = store.lcboKey
    def showI(store: IStore): P_KEY = store.pKey
  }

  // extension method style (object.showKeyPairVals)
  implicit final class ShowKeyPairOps[E,I,A](a: A)(implicit ev: ShowKeyPair[E, I, A]) {
    def showKeyPairVals: ShowKeyPairVals[E, I] = ev.showKeyPairVals(a)
  }
}
