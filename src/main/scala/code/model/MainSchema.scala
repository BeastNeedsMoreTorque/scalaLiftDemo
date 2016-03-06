package code.model

import org.squeryl.dsl.CompositeKey2

import org.squeryl.{KeyedEntity, ForeignKeyDeclaration, Schema}
import net.liftweb.squerylrecord.RecordTypeMode._

class Inventory(val storeid: Long, val productid: Long, var quantity: Long) extends KeyedEntity[CompositeKey2[Long,Long]] {
  def id = compositeKey(storeid, productid)

  def isDirty(inv: InventoryAsLCBOJson): Boolean =
    quantity != inv.quantity

  def copyAttributes(inv: InventoryAsLCBOJson): Inventory = {
    quantity = inv.quantity
    this
  }
}
/**
  * Created by philippederome on 2016-01-01.
  *   // References:
  *
  * @see http://stackoverflow.com/questions/12794427/squeryl-and-postgresqls-autoincrement/12876399#12876399
  * @see http://stackoverflow.com/questions/28859798/in-scala-how-can-i-get-plays-models-and-forms-to-play-nicely-with-squeryl-and?answertab=active#tab-top
  */
object MainSchema extends Schema {
  val stores = table[Store]("store")

  val products = table[Product]("product")
  // in Postgres:  CREATE SEQUENCE s_product_id;  See output from printDdl in Boot.
  // alter table product alter column id set default nextval('s_product_id');
  // String columns are typically nullable.

  val inventories = manyToManyRelation(stores, products).
    via[Inventory]((s,p,inv) => (inv.storeid === s.id, p.id === inv.productid))

  val userProducts = table[UserProduct]("userproduct")
  // In Postgres: CREATE SEQUENCE s_userproduct_id;
  // alter table userproduct alter column id set default nextval('s_userproduct_id');

  val productToUserProducts = oneToManyRelation(products, userProducts).
    via((p,u) => p.id === u.productid)
  // Foreign-key constraints:
  // "userproductFK1" FOREIGN KEY (productid) REFERENCES product(id)
  //    alter table "userproduct" add constraint "userproductFK1" foreign key ("productid") references "product"("id");

  val productToInventories = oneToManyRelation(products, inventories).
    via((p,s) => p.id === s.productid)
  // alter table "inventory" add constraint "inventoryFK2" foreign key ("productid") references "product"("id")

  val storeToInventories = oneToManyRelation(stores, inventories).
    via((s,inv) => s.id === inv.storeid)
  //alter table "inventory" add constraint "inventoryFK3" foreign key ("storeid") references "store"("id");

  // the default constraint for all foreign keys in this schema :
  override def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration) =
  foreignKeyDeclaration.constrainReference

  on(stores) { s =>
    declare(
      s.lcbo_id defineAs (unique,indexed("store_lcbo_id_idx")))
  }

  on(userProducts) { up =>
    declare(
      up.productid defineAs indexed("userproduct_product"),
      up.userid defineAs indexed("userproduct_user"),
      columns(up.userid, up.productid ) are(unique,indexed("user_prod_idx")))

  }

  on(products) { p =>
    declare(
      p.lcbo_id defineAs (unique,indexed("lcbo_id_idx")),
      p.primary_category defineAs indexed("pr_category_id_idx"))
  }
}
