package code.model

import net.liftweb.common.Loggable
import net.liftweb.mapper._

import scala.util.Try

// this code makes a case for value of unit testing since it took me a while to get it right... Let's set it up!
object UserProducts extends UserProducts with LongKeyedMetaMapper[UserProducts] with Loggable {
  /**
    * The join table UserProducts is increasing by one the row matching user in Users and prod in DBProduct or initialized to 1 on creation of relationship.
    * @param user the database user row consuming the product
    * @param prod the database DBProduct row that has its consumption increased by the user
    * @return a Try on the user's first name as is registered and the count of times the product has been consumed, accounting for this consumption.
    */
  def consume(user: User, prod: DBProduct): Try[(String, Long)] = {
    // get a UserProducts list matching by user then by product id, that is meant to be of size 0-1 but theoretically more if buggy.
    Try {
      val userProdsList = UserProducts.
        findAll(By(UserProducts.user, user.id.get)).
        filter(_.product.get == prod.id.get)

      // when list is non-empty update entry by incrementing by one else create/insert a new entry to DB
      val elem = userProdsList.headOption.map { first =>
        first.selectionsCount.set(first.selectionsCount.get + 1)
        first.saveMe()
        (first.user.toOption.get.firstName.get, first.selectionsCount.get)
      }
      elem.getOrElse {
        // create/insert new entry
        UserProducts.create.
          user(user.id.get).
          product(prod.id.get).
          saveMe() // persist on insert specifying minimal info with defaults specified by case class, so here only two columns are explicit to satisfy FK.
        (user.firstName.get, 1.toLong)
      }
    }
  }
}

class UserProducts extends LongKeyedMapper[UserProducts] with CreatedUpdated {
  def getSingleton = UserProducts

  def primaryKeyField = id

  object id extends MappedLongIndex(this)

  // our own auto-generated id

  object user extends MappedLongForeignKey(this, User)

  object product extends MappedLongForeignKey(this, DBProduct)

  object selectionsCount extends MappedLong(this) {
    override def defaultValue = 1
  }

  object review extends MappedText(this)

}


