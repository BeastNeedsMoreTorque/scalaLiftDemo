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
    * persist a product to database handling insert or update depending on whether the entry exists already or not.
    * Efficiency consideration: when doing two writes, use DB.use to avoid round-trips.
    * Atomicity provided by liftweb in boot.scala (normally would be S.addAround(DB.buildLoanWrapper)), but done differently for Squeryl specifically.
    * @param p a product representing the Record object that was created after serialization from LCBO.
    * @see Lift in Action, Chapter 10-11 (Mapper and mostly Record), Section 10.3.2 Transactions
    * @return the user who requested the product and the number of times the user has purchased this product as a pair/tuple.
    *         May throw but would be caught as a Failure within Box to be consumed higher up.
    */
  def persist(p: Product) = {
    User.currentUser.dmap { Failure("unable to store transaction, Login first!").asA[(String, Long)] }
    { user => // normal case
      // update it with new details; we could verify that there is a difference between LCBO and our version first...
      // assume price and URL for image are fairly volatile and rest is not. In real life, we'd compare them all to check.
      // Use openOr on Box prod so that if non-empty, we update it, otherwise we create and save the product.
      // tryo captures database provider errors (column size too small for example, reporting it as an Empty Box with Failure)
      tryo {
        DB.use(DefaultConnectionIdentifier) { connection =>
          // avoids two/three round-trips to store to DB. Tested this with some long sleep before UserProduct.consume and saw old timestamp for Product compared with UserProduct
          // and it got stored at same time as UserProduct (monitoring Postgres).
          // We do this in transaction so we have local consistency (i.e. the product will not be deleted by some other transaction while we're here)
          val prod: Box[Product] = products.where(_.lcbo_id === p.lcbo_id).forUpdate.headOption
          // Assumes it has been synched up elsewhere if needed, not our business here (or go directly to cache). Squeryl very friendly DSL syntax!
          var count = 0.toLong
          prod.map { q =>
            val userProd: Box[UserProduct] = userProducts.where(u => u.user_c === user.id.get and u.productid === q.id).forUpdate.headOption
            if (userProd.isEmpty) {
              // (Product would be stored in DB with no user interest)
              count = 1
              UserProduct.createRecord.user_c(user.id.get).productid(q.id).selectionscount(count).save // cascade save dependency.
            } else {
              // cascade save dependency (there should only be one entry to update).
              userProd.map { u =>
                count = u.selectionscount.get + 1
                u.selectionscount.set(count)
                u.updated.set(u.updated.defaultValue)
                u.update // Active Record pattern )
              } // from compiler perspective the map could have been a no-op, but that's not really possible in practice.
            }
          } openOr {
            count = 1 // we never saw that product before and user shows interest, store both.
            p.save
            UserProduct.createRecord.user_c(user.id.get).productid(p.id).selectionscount(count).save // cascade save dependency.
          }
          (user.firstName.get, count)
        }
      }
    }
  }

  /**
    * Purchases a product by increasing user-product count (amount) in database as a way to monitor usage..
    * @param product contains a product
    * @return a Box capturing any exception to be reported further up, capturing how many times user has consumed product.
    */
  def consume(product: Product): Box[(String, Long)] = persist(product) // yeah, could do other things such as real payment transaction and exchange of asset.

}




