package code.model

import code.model.GlobalLCBO_IDs._
import code.model.ShowKeyPair.ShowKeyPairVals
import code.model.utils.ShowKey

// patterned after Show type class, it is meant to show a pair of "keys" from A, one external one internal
trait ShowKeyPair[Internal, A] extends ShowKey[A] {
  def showE(a: A): LCBO_KEY
  def showI(a: A): Internal
  def showKeyPairVals(a: A): ShowKeyPairVals[Internal] = ShowKeyPairVals(showE(a), showI(a))
  override def show(a: A): Long = showE(a)
}

object ShowKeyPair {

  case class ShowKeyPairVals[I](eKey: LCBO_KEY, iKey: I)
  def apply[I, A](implicit s: ShowKeyPair[I, A]): ShowKeyPair[I, A] = s

  def pure[I, A](fe: A => LCBO_KEY, fi: A => I): ShowKeyPair[I, A] = new ShowKeyPair[I, A] {
    def showE(x: A): LCBO_KEY = fe(x)
    def showI(x: A): I = fi(x)
  } // from Daniel Westheide talk on type classes

  implicit val productShowKeyPair: ShowKeyPair[P_KEY, IProduct] = pure(_.lcboKey, _.pKey)
  implicit val storeShowKeyPair: ShowKeyPair[P_KEY, IStore] = pure(_.lcboKey, _.pKey)

  // extension method style (object.showKeyPairVals)
  implicit final class ShowKeyPairOps[I,A](a: A)(implicit ev: ShowKeyPair[I, A]) {
    def showKeyPairVals: ShowKeyPairVals[I] = ev.showKeyPairVals(a)
  }
}
