package code.model

import net.liftweb.record.field.LongField
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord
import org.squeryl.annotations.Column

class UserStore private() extends Record[UserStore] with KeyedRecord[Long] with Created[UserStore] {
  def meta = UserStore

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  lazy val store = MainSchema.storeToUserStores.right(this)

  val userid = new LongField(this)
  val storeid = new LongField(this)
}

object UserStore extends UserStore with MetaRecord[UserStore] {}




