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

  val productToUserProducts = oneToManyRelation(products, userProducts).
    via((p,s) => p.id === s.productid)

  on(userProducts) { s =>
    declare(s.productid defineAs indexed("product_idx"))
  }

  on(products) { p =>
    declare(p.primary_category defineAs indexed("pr_category_id_idx"))
  }
}
