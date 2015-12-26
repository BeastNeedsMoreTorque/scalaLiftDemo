package code.model

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.common.Failure
import net.liftweb.util.Helpers.tryo

/**
  * Created by philippederome on 15-11-01.
  * DBProduct: The elements of a product from LCBO catalogue that we deem of relevant interest to replicate in DB for this toy demo.
  * A sister of class Product.
  * Limitations: there is no effort at transaction management.
  */
class DBProduct extends LongKeyedMapper[DBProduct] with CreatedUpdated  {

  def getSingleton = DBProduct

  def primaryKeyField = id

  object id extends MappedLongIndex(this)


  // our own auto-generated id

  object lcbo_id extends MappedInt(this)

  object is_discontinued extends MappedBoolean(this)

  object `package` extends MappedString(this, 80)

  object total_package_units extends MappedInt(this)

  object primary_category extends MappedString(this, 40)

  object name extends MappedString(this, 80)

  object image_thumb_url extends MappedString(this, 200)

  object origin extends MappedString(this, 200)

  object price_in_cents extends MappedInt(this)

  object alcohol_content extends MappedInt(this)

  object volume_in_milliliters extends MappedInt(this)

}

/**
  * object DBProduct: supports persist that does insert or update, depending whether we already have a copy of LCBO product in our database
  * Takes care of a dependency when persisting with assumption that this is always bound to a valid user request, so will attempt to store
  * to UserProducts as well.
  * @todo : Errors are possible if data is too large to fit. See to improve error handling.
  *       The framework traits enable creating the table or altering it based on definitions of the class automatically (at least in dev).
  */
object DBProduct extends DBProduct
with LongKeyedMetaMapper[DBProduct] with Loggable {
  override def dbTableName = "product"
  /**
    * persist a product to database handling insert or update depending on whether the entry exists already or not.
    * Efficiency consideration: when doing two writes, use DB.use to avoid round-trips.
    * Atomicity provided by liftweb in boot.scala with     S.addAround(DB.buildLoanWrapper)
    * @param p a product representing the JSON object that was serialized from LCBO.
    * @see Lift in Action, Chapter 10 Persistence with Mapper, Section 10.3.2 Transactions
    * @return the user who requested the product and the number of times the user has purchased this product as a pair/tuple.
    *         May throw
    */
  def persist(p: Product): Box[(String, Long)] = {
    def createProduct: DBProduct = {
      // store in same format as received by provider so that un-serializing if required will be same logic.
      DBProduct.create.
        lcbo_id(p.id).
        name(p.name).
        primary_category(p.primary_category).
        is_discontinued(p.is_discontinued).
        `package`(p.Package).
        origin(p.origin).
        image_thumb_url(p.image_thumb_url).
        price_in_cents(p.price_in_cents).
        total_package_units(p.total_package_units).
        volume_in_milliliters(p.volume_in_milliliters).
        alcohol_content(p.alcohol_content).
        saveMe
    }

    User.currentUser.dmap {
      val fail: Box[(String, Long)] = Failure("unable to store transaction, Login first!")
      fail
    } { user => // normal case
      val prod = DBProduct.find(By(DBProduct.lcbo_id, p.id)) // db column lcbo_id matches memory id (same name as JSON field)
      // update it with new details; we could verify that there is a difference between LCBO and our version first...
      // assume price and URL for image are fairly volatile and rest is not. In real life, we'd compare them all to check.
      // Use openOr on Box prod so that if non-empty, we update it, otherwise we create and save the product.
      tryo {
        DB.use(DefaultConnectionIdentifier) {
          // avoids two round-trips to store to DB. Tested this with some long sleep before UserProduct.consume and saw old timestamp for Product compared with UserProduct
          // and it got stored at same time as UserProduct (monitoring Postgres).
          connection =>
            val newProd = prod.map(_.price_in_cents(p.price_in_cents).
              image_thumb_url(p.image_thumb_url).
              saveMe()) openOr createProduct
            UserProduct.consume(user, newProd) // once the product has been saved, also save the UserProducts relationship for an additional count of the product for the user.
        }
      }
    }
  }
}
