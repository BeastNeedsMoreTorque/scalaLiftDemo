package code.model

import net.liftweb.common.Loggable
import net.liftweb.mapper.SelectableField

import scala.util.Try

/**
  * Created by philippederome on 15-11-15.
  */
trait PersistProductIDAsDB extends Loggable {
  self: ProductProvider =>
  def consumedProducts: Set[Int] = {
    Try {
      DBProduct.findAllFields(Seq[SelectableField](DBProduct.lcbo_id)).map(_.lcbo_id.get)
    }
    match {
      // transforms Try[List[Int]] to a Set[Int], capturing exception error to log.
      case util.Success(x) => x.toSet
      case util.Failure(ex) =>
        logger.error(s"consumedProducts failed to load from database:$ex") // shout it loud, don't swallow error (db connection problem perhaps).
        Set.empty[Int]
    }
  }

  /**
    *
    * persists product to corresponding database table.
    * throws most likely, be prepared to consume errors further up (duplicate primary key, connection down, etc).
    * @param product the case class that is a subset of the LCBO JSON product data
    */
  def persist(product: Product): Try[(String, Long)] = DBProduct.persist(product)
}
