package code.model

import MainSchema._
import net.liftweb.db.DB
import net.liftweb.record.field.{LongField,StringField,BooleanField,IntField}
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.common._
import net.liftweb.common.Failure
import net.liftweb.util.DefaultConnectionIdentifier
import net.liftweb.util.Helpers.tryo
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.squerylrecord.KeyedRecord
import org.squeryl.annotations.Column

/**
  * Created by philippederome on 15-11-01. Modified 16-01-01 for Record+Squeryl (to replace Mapper), Record being open to NoSQL and Squeryl providing ORM service.
  * DBProduct: The elements of a product from LCBO catalogue that we deem of relevant interest to replicate in DB for this toy demo.
  * A sister of class Product.
  * Some StringFields take defaults as we specify not null in database but could receive null from LCBO (esp. relevant for image_thumb_url)
  * name has no default.
  */
class DBProduct private() extends Record[DBProduct] with KeyedRecord[Long] with CreatedUpdated[DBProduct]  {
  def meta = DBProduct

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id, using a Sequence known/assumed to Squeryl .

  val lcbo_id = new IntField(this) // indexed?
  val is_discontinued = new BooleanField(this, false)
  val `package` = new StringField(this, 80)
  val total_package_units = new IntField(this)
  val primary_category = new StringField(this, 40)
  val name = new StringField(this, 80)
  val image_thumb_url = new StringField(this, 200)
  val origin = new StringField(this, 200)
  val price_in_cents = new IntField(this)
  val alcohol_content = new IntField(this)
  val volume_in_milliliters = new IntField(this)
}

/**
  * object DBProduct: supports persist that does insert or update, depending whether we already have a copy of LCBO product in our database
  * Takes care of a dependency when persisting with assumption that this is always bound to a valid user request, so will attempt to store
  * to UserProducts as well.
  * Errors are possible if data is too large to fit. tryo will catch those and report them.
  */
object DBProduct extends DBProduct with MetaRecord[DBProduct]  {
  /**
    * persist a product to database handling insert or update depending on whether the entry exists already or not.
    * Efficiency consideration: when doing two writes, use DB.use to avoid round-trips.
    * Atomicity provided by liftweb in boot.scala (normally would be S.addAround(DB.buildLoanWrapper)), but done differently for Squeryl specifically.
    * @param p a product representing the JSON object that was serialized from LCBO.
    * @see Lift in Action, Chapter 10-11 (Mapper and mostly Record), Section 10.3.2 Transactions
    * @return the user who requested the product and the number of times the user has purchased this product as a pair/tuple.
    *         May throw but would be caught as a Failure within Box to be consumed higher up.
    */
  def persist(p: Product) = {
    def create = {
      // store in same format as received by provider so that un-serializing if required will be same logic.
      DBProduct.createRecord.
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
        alcohol_content(p.alcohol_content)
    }

    User.currentUser.dmap {
      Failure("unable to store transaction, Login first!").asA[(String, Long)]
    } { user => // normal case
      // update it with new details; we could verify that there is a difference between LCBO and our version first...
      // assume price and URL for image are fairly volatile and rest is not. In real life, we'd compare them all to check.
      // Use openOr on Box prod so that if non-empty, we update it, otherwise we create and save the product.
      // tryo captures database provider errors (column size too small for example, reporting it as an Empty Box with Failure)
      tryo {
       DB.use(DefaultConnectionIdentifier) { connection =>
         // avoids two/three round-trips to store to DB. Tested this with some long sleep before UserProduct.consume and saw old timestamp for Product compared with UserProduct
         // and it got stored at same time as UserProduct (monitoring Postgres).
         // First query before update as update does not give us the PK for the product (a cost consequence for not using same PK as LCBO).
         // We do this in transaction so we have local consistency (i.e. the product will not be deleted by some other transaction while we're here)
         val prod: Box[DBProduct] = products.where(_.lcbo_id === p.id).headOption  // Squeryl very friendly DSL syntax! db column lcbo_id matches memory id (same name as JSON field)
         val prodId = prod.map { q =>
           update(products)(q =>
             where(q.lcbo_id === p.id)  // p.id is the id from LCBO's JSON's perspective whereas t.id is our PK from our own database.
               set(q.price_in_cents := p.price_in_cents,
               q.image_thumb_url := p.image_thumb_url))
           q.id
         } openOr {
           val q = create
           q.save
           q.id
         }
         UserProduct.consume(user, prodId)   // once the product has been saved, also save the UserProducts relationship for an additional count of the product for the user.
        }
      }
    }
  }
}
