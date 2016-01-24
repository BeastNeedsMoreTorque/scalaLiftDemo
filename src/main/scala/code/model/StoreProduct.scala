package code.model

import code.model.MainSchema._
import net.liftweb.record.field.{LongField,IntField}
import net.liftweb.record.{Record, MetaRecord}
import net.liftweb.squerylrecord.KeyedRecord
import net.liftweb.util.Props
import org.squeryl.annotations.Column

import scala.annotation.tailrec
import scala.collection.Iterable

class StoreProduct private() extends Record[StoreProduct] with KeyedRecord[Long] with CreatedUpdated[StoreProduct] {
  def meta = StoreProduct

  @Column(name="id")
  override val idField = new LongField(this, 1)  // our own auto-generated id

  lazy val product = MainSchema.productToStoreProducts.right(this)

  val storeid = new LongField(this)
  val productid = new LongField(this)
  val inventory = new IntField(this)
}

object StoreProduct extends StoreProduct with MetaRecord[StoreProduct] {
  private val DBBatchSize = Props.getInt("storeProduct.DBBatchSize", 1)

  def create(storeId: Long, p: Product): StoreProduct = {
    createRecord.storeid(storeId).productid(p.id).inventory(p.inventory.get)
  }

  // simply insert storeid, productid but also the inventory, that is the Int carried in myProducts 2nd component.
  @tailrec
  def insertStoreProducts(storeId: Long, myProducts: Iterable[(Product, Int)]): Unit = {
    val slice: Iterable[StoreProduct] = myProducts.take(DBBatchSize).map(x =>  create(storeId, x._1) )
    storeProducts.insert(slice)
    val rest = myProducts.takeRight(myProducts.size - slice.size)
    if (!rest.isEmpty) insertStoreProducts(storeId, rest)
  }

}




