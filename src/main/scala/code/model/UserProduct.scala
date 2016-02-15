package code.model

import code.model.MainSchema._
import net.liftweb.common.{Box, Failure}
import net.liftweb.db.DB
import net.liftweb.record.field.{LongField,StringField}
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.util.DefaultConnectionIdentifier
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.util.Helpers._

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
object UserProduct extends UserProduct with MetaRecord[UserProduct] {

  /**
    * consume (persist) a product to database handling insert or update depending on whether the entry exists already or not.
    * Efficiency consideration: when doing two writes, use DB.use to avoid round-trips.
    * Atomicity provided by liftweb in boot.scala (normally would be S.addAround(DB.buildLoanWrapper)), but done differently for Squeryl specifically.
    *
    * @param p a product representing the Record object that was created after serialization from LCBO.
    * @see Lift in Action, Chapter 10-11 (Mapper and mostly Record), Section 10.3.2 Transactions
    * @return the user who requested the product and the number of times the user has purchased this product as a pair/tuple.
    *         May throw but would be caught as a Failure within Box to be consumed higher up.
    */
  def consume(p: Product, quantity: Int): Box[(String, Long)] = {
    User.currentUser.dmap { Failure("unable to store transaction, Login first!").asA[(String, Long)] }
    { user => // normal case
      // update it with new details; we could verify that there is a difference between LCBO and our version first...
      // assume price and URL for image are fairly volatile and rest is not. In real life, we'd compare them all to check.
      // tryo captures database provider errors (column size too small for example, reporting it as an Empty Box with Failure)
      tryo {
        DB.use(DefaultConnectionIdentifier) { connection =>
          // avoids two/three round-trips to store to DB.
          // We do this in transaction so we have local consistency (i.e. the product will not be deleted by some other transaction while we're here)
          val prod = products.where(_.lcbo_id === p.lcbo_id).forUpdate.headOption
          // Assumes it has been synched up elsewhere if needed, not our business here (or go directly to cache). Squeryl very friendly DSL syntax!

          val updatedCount = prod.fold {
            // we never saw that product before and user shows interest, store both. It could theoretically happen if user selects a product with many new products and cache warm up is especially slow.
            p.save
            UserProduct.createRecord.user_c(user.id.get).productid(p.id).selectionscount(quantity).save // cascade save dependency.
            quantity.toLong
          } { q =>
            val userProd = userProducts.where(u => u.user_c === user.id.get and u.productid === q.id).forUpdate.headOption
            userProd.fold {
              // (Product would be stored in DB with no previous user interest)
              UserProduct.createRecord.user_c(user.id.get).productid(q.id).selectionscount(quantity).save // cascade save dependency.
              quantity.toLong
            } { u: UserProduct=>
                val newCount = u.selectionscount.get + quantity
                u.selectionscount.set(newCount)
                u.updated.set(u.updated.defaultValue)
                u.update // Active Record pattern )
                newCount
            }
          }
          (user.firstName.get, updatedCount)
        }
      }
    }
  }

}