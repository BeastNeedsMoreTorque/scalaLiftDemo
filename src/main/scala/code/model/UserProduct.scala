package code.model

import MySchema._
import net.liftweb.record.field.{LongField,StringField}
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.squerylrecord.KeyedRecord
import org.squeryl.annotations.Column

class UserProduct private() extends Record[UserProduct] with KeyedRecord[Long] with CreatedUpdated[UserProduct] {
  def meta = UserProduct

  @Column(name="id")
  override val idField = new LongField(this)  // our own auto-generated id

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
  def consume(user: User, prod: DBProduct): (String, Long) = {
    // get fist UserProduct item matching if available (don't care if there are multiple matches, we use a PK to query!).
    val userProd: Option[UserProduct] = from(userProducts)(uProd =>
      where(uProd.user_c === user.id.get and uProd.product === prod.id) select (uProd)).headOption
    if (userProd.isDefined) {
    //  val up = userProd.get
     // up.selectionsCount.set(up.selectionsCount.get + 1)
      //up.update()
      update(userProducts)(up =>
          where(up.user_c === user.id.get and up.product === prod.id)
          set(up.selectionscount  := up.selectionscount + 1)
      )
      (user.firstName.get, userProd.get.selectionscount.get +1)
    } else {
      // create/insert new entry
      UserProduct.createRecord.
        user_c(user.id.get).
        product(prod.id).save
      (user.firstName.get, 1.toLong)
    }
  //  val elem = userProd.map { up: UserProduct =>
   //   up.selectionsCount.set(up.selectionsCount.get + 1)
   //   up.update()
   //   (user.firstName.get, up.selectionsCount.get)
   // }
  //  elem.getOrElse {
      // create/insert new entry
  //    UserProduct.createRecord.
  //      user_c(user.id.get).
   //     product(prod.id.get).save
   //   (user.firstName.get, 1.toLong)
   // }

    //   val userProdsList = UserProduct.
  //    findAll(By(UserProduct.user, user.id.get)) //.
    //  filter(_.product.get == prod.id.get)
      // when list is non-empty update entry by incrementing by one else create/insert a new entry to DB
   //   val elem = userProdsList.headOption.map { first =>
   //     first.selectionsCount.set(first.selectionsCount.get + 1)
    //    first.saveMe()
   //     (first.user.toOption.get.firstName.get, first.selectionsCount.get)
   //   }
   //   elem.getOrElse {
        // create/insert new entry
     //   UserProduct.create.
      //    user(user.id.get).
      //    product(prod.id.get).
      //    saveMe() // persist on insert specifying minimal info with defaults specified by case class, so here only two columns are explicit to satisfy FK.
    //    (user.firstName.get, 1.toLong)
  //    }
  }
}




