package code.model

/**
  * Created by philippederome on 2016-08-02.
  */
trait Equivalent[T] {
  def equivalent(other: T): Boolean
}
