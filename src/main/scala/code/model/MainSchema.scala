package code.model

import org.squeryl.{ForeignKeyDeclaration, Schema}
import net.liftweb.squerylrecord.RecordTypeMode._

/**
  * Created by philippederome on 2016-01-01.
  *   // References:
  *
  * @see http://stackoverflow.com/questions/12794427/squeryl-and-postgresqls-autoincrement/12876399#12876399
  * @see http://stackoverflow.com/questions/28859798/in-scala-how-can-i-get-plays-models-and-forms-to-play-nicely-with-squeryl-and?answertab=active#tab-top
  */
object MainSchema extends Schema {
  val stores = table[Store]("store")
  // CREATE SEQUENCE s_store_pkid;
  // alter table store alter column pkid set default nextval('s_store_pkid');

  val products = table[Product]("product")
  // in Postgres:  CREATE SEQUENCE s_product_pkid;  See output from printDdl in Boot.
  // alter table product alter column pkid set default nextval('s_product_pkid');
  // String columns are typically nullable.

  val inventories = manyToManyRelation(stores, products).
    via[Inventory]((s,p,inv) => (inv.storeid === s.idField.get, p.idField.get === inv.productid))
  // The table in database needs to be called "Inventory".
  //alter table "Inventory" add primary key ("storeid","productid");

  val userProducts = table[UserProduct]("userproduct")
  // In Postgres: CREATE SEQUENCE s_userproduct_id;
  // alter table userproduct alter column id set default nextval('s_userproduct_id');

  val productToUserProducts = oneToManyRelation(products, userProducts).
    via((p,u) => p.id === u.productid)
  // Foreign-key constraints:
  // "userproductFK1" FOREIGN KEY (productid) REFERENCES product(pkid)
  //    alter table "userproduct" add constraint "userproductFK1" foreign key ("productid") references "product"("pkid");

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
      p.lcbo_id defineAs (unique,indexed("lcbo_id_idx")))
  }
}
