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

  val products = table[Product]("product")
  // String columns are typically nullable.

  val inventories = manyToManyRelation(stores, products, "inventory").
    via[Inventory]((s,p,inv) => (inv.storeid.underlying === s.idField.get, p.idField.get === inv.productid.underlying))
  // The table in database needs to be called "Inventory". See Inventory class for all columns.

  val userProducts = table[UserProduct]("userproduct")

  val productToUserProducts = oneToManyRelation(products, userProducts).
    via((p,u) => p.id === u.productid)
  // Foreign-key constraints (see end of file):

  // the default constraint for all foreign keys in this schema :
  override def applyDefaultForeignKeyPolicy(foreignKeyDeclaration: ForeignKeyDeclaration): Unit =
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
