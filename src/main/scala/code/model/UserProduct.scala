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

  val user_c = new LongField(this)
  val product = new LongField(this)
  val selectionscount = new LongField(this) {
    override def defaultValue = 1
  }
  val review = new StringField(this, "")
}
// this code makes a case for value of unit testing since it took me a while to get it right... Let's set it up!
object UserProduct extends UserProduct with MetaRecord[UserProduct] {
  /**
    * The join table UserProducts is increasing by one the row matching user in Users and prod in DBProduct or initialized to 1 on creation of relationship.
    * @param user the database user row consuming the product
    * @param prod the database DBProduct row that has its consumption increased by the user
    * @return
    *         Exception Handling is done at a higher level since this is an intermediate table for Many-Many relationship
    *         between User and Product; higher level app code should not come here directly.
    */
  def consume(user: User, productId: Long): (String, Long) = {
    // get fist UserProduct item matching if available (don't care if there are multiple matches, we use effectively a pseudo-key to query!).
    val userProd: Box[UserProduct] = userProducts.where( uProd => uProd.user_c === user.id.get and uProd.product === productId).headOption
    val count = userProd.map { s =>
      s.selectionscount.set(s.selectionscount.get + 1)
      s.update
      s.selectionscount.get
    } openOr { UserProduct.createRecord.user_c(user.id.get).product(productId).save       // create/insert new entry
      1.toLong
    }
    (user.firstName.get, count)
  }
}




