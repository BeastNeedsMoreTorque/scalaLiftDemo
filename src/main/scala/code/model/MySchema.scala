package code.model
import org.squeryl.Schema
/**
  * Created by philippederome on 2016-01-01.
  */
object MySchema extends Schema {
  val products = table[DBProduct]("product")
  // in Postgres:  CREATE SEQUENCE s_product_id;
  // alter table product alter column id set default nextval('s_product_id');
  // http://stackoverflow.com/questions/12794427/squeryl-and-postgresqls-autoincrement/12876399#12876399
  //  http://stackoverflow.com/questions/28859798/in-scala-how-can-i-get-plays-models-and-forms-to-play-nicely-with-squeryl-and?answertab=active#tab-top

  val userProducts = table[UserProduct]("userproduct")
  // In Postgres: CREATE SEQUENCE s_userproduct_id;
  // alter table userproduct alter column id set default nextval('s_userproduct_id');

}
