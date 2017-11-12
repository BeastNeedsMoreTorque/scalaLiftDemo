package code.model

import java.util.Calendar

import net.liftweb.record.field.DateTimeField
import net.liftweb.record.Record

/**
  * Created by philippederome on 2016-01-02. Credit Lift Cookbook.
  */

trait Created[T <: Created[T]] extends Record[T] {
  self: T => // see https://tpolecat.github.io/2015/04/29/f-bounds.html, forces the subtype of Created (concrete type) to be a T
  // as opposed to an arbitrary case class (F-bounded polymorphism)
  val created: DateTimeField[T] = new DateTimeField(this) {
    override def defaultValue = Calendar.getInstance
  }
}

trait Updated[T <: Updated[T]] extends Record[T] {
  self: T =>

  val updated = new DateTimeField(this) {
    override def defaultValue = Calendar.getInstance
  }

  def onUpdate: Unit = this.updated(Calendar.getInstance)

}

trait CreatedUpdated[T <: Updated[T] with Created[T]] extends
Updated[T] with Created[T] {
  self: T =>
}
