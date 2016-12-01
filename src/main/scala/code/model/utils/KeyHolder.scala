package code.model.utils

trait KeyHolder[A] { // effectively resembles Show[A] a great deal, but is meant to show a small aspect of the type A, not the whole thing.
  def getKey(f: A): String
}

object KeyHolder {
  def getKey[A](f: A => String): KeyHolder[A] = new KeyHolder[A] {
    def getKey(a: A): String = f(a)
  }
}

