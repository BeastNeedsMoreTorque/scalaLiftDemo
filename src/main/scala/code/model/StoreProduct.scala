package code.model

import code.model.MainSchema._
import net.liftweb.db.DB
import net.liftweb.record.field.LongField
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.RecordTypeMode._
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.util.DefaultConnectionIdentifier
import org.squeryl.annotations.Column

class StoreProduct private() extends Record[StoreProduct] with KeyedRecord[Long] with CreatedUpdated[StoreProduct] {
  def meta = StoreProduct

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  lazy val product = MainSchema.productToStoreProducts.right(this)

  val storeid = new LongField(this)
  val productid = new LongField(this)
  val inventory = new LongField(this)
}

object StoreProduct extends StoreProduct with MetaRecord[StoreProduct] {

  def insertIfNone(storeId: Long, prodId: Long, inventory: Long) = {
    DB.use(DefaultConnectionIdentifier) { connection =>
      val rec = storeProducts.
        where( (sp: StoreProduct) => sp.storeid === storeId and sp.productid === prodId ).
        forUpdate.headOption // Load from DB if available, else create it Squeryl very friendly DSL syntax!
      rec.fold  {
        val q = createRecord.storeid(storeId).productid(prodId).inventory(inventory)
        q.save
        q
      } { identity // don't try to update for now
      }
    }
  }


}




