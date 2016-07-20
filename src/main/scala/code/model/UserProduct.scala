package code.model

import net.liftweb.record.field.LongField
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord

import org.squeryl.annotations.Column

class UserProduct private() extends Record[UserProduct] with KeyedRecord[Long] with CreatedUpdated[UserProduct] {
  def meta: MetaRecord[UserProduct] = UserProduct

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  val userid = new LongField(this)
  val productid = new LongField(this)
  val selectionscount = new LongField(this) {
    override def defaultValue = 1
  }
}

object UserProduct extends UserProduct with MetaRecord[UserProduct] {}
