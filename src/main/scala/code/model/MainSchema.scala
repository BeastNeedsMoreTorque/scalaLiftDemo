package code.model
import org.squeryl.Schema
import net.liftweb.squerylrecord.RecordTypeMode._

/**
  * Created by philippederome on 2016-01-01.
  *   // References:
  * @see http://stackoverflow.com/questions/12794427/squeryl-and-postgresqls-autoincrement/12876399#12876399
  * @see http://stackoverflow.com/questions/28859798/in-scala-how-can-i-get-plays-models-and-forms-to-play-nicely-with-squeryl-and?answertab=active#tab-top
  */
object MainSchema extends Schema {
  val stores = table[Store]("store")

  val products = table[Product]("product")
  // in Postgres:  CREATE SEQUENCE s_product_id;  See output from printDdl in Boot.
  // alter table product alter column id set default nextval('s_product_id');
  // String columns are typically nullable.

  val userProducts = table[UserProduct]("userproduct")
  // In Postgres: CREATE SEQUENCE s_userproduct_id;
  // alter table userproduct alter column id set default nextval('s_userproduct_id');

  val storeProducts = table[StoreProduct]("storeproduct")

  val userStores = table[UserStore]("userstore")

  val productToUserProducts = oneToManyRelation(products, userProducts).
    via((p,u) => p.id === u.productid)

  val storeToUserStores = oneToManyRelation(stores, userStores).
    via((s,u) => s.id === u.storeid)

  val productToStoreProducts = oneToManyRelation(products, storeProducts).
    via((p,s) => p.id === s.productid)

  on(stores) { s =>
    declare(
      s.lcbo_id defineAs (unique,indexed("store_lcbo_id_idx")))
  }

  on(userProducts) { up =>
    declare(
      up.productid defineAs indexed("userproduct_product"),
      up.user_c defineAs indexed("userproduct_user_c"),
      columns(up.user_c, up.productid ) are(unique,indexed("user_prod_idx")))
      // Foreign-key constraints:
      // "userproductFK1" FOREIGN KEY (productid) REFERENCES product(id)
  }

  on(storeProducts) { sp =>
    declare(
      sp.productid defineAs indexed("product_idx"),
      sp.storeid defineAs indexed("store_idx"),
      columns(sp.storeid, sp.productid) are(unique, indexed("storeproduct_idx")))
    //  alter table storeproduct add constraint storeproductFK1 foreign key (productid) references product (lcbo_id) match full;
    //  alter table storeproduct add constraint storeproductFK2 foreign key (storeid) references store (lcbo_id) match full;

  }

  on(products) { p =>
    declare(
      p.lcbo_id defineAs (unique,indexed("lcbo_id_idx")),
      p.primary_category defineAs indexed("pr_category_id_idx"))
  }
}
