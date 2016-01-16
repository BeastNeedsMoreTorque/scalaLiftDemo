package code.model

import net.liftweb.record.field.LongField
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord
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
object StoreProduct extends StoreProduct with MetaRecord[StoreProduct] {}




