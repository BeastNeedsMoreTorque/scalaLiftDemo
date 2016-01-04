package code.model

import MainSchema._
import net.liftweb.common._
import net.liftweb.record.field.{LongField,StringField}
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.squerylrecord.KeyedRecord
import org.squeryl.annotations.Column

class UserProduct private() extends Record[UserProduct] with KeyedRecord[Long] with CreatedUpdated[UserProduct] {
  def meta = UserProduct

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  lazy val product = MainSchema.productToUserProducts.right(this)

  val user_c = new LongField(this)
  val productid = new LongField(this)
  val selectionscount = new LongField(this) {
    override def defaultValue = 1
  }
  val review = new StringField(this, "")
}
// this code makes a case for value of unit testing since it took me a while to get it right... Let's set it up!
object UserProduct extends UserProduct with MetaRecord[UserProduct] {}




