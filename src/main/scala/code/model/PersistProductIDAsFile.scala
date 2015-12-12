package code.model

import java.io.{FileNotFoundException, IOException}

import net.liftweb.common.Loggable
import net.liftweb.util.Helpers._

import scala.io.Source
import scala.util.Try

/**
  * Created by philippederome on 15-11-06.  NOT USED ANYMORE! Just to show flexibility of traits and how components plug in together. Was the first implementation.
  */
trait PersistProductIDAsFile extends Loggable {
  self: ProductProvider =>
  val ConsumedProductsFile = "consumedProducts"

  def consumedProducts: Set[Int] = {
    Try {
      Source.fromFile(ConsumedProductsFile).getLines.toList
    }
    match {
      // transforms Try[List[String]] to a Set[Int], capturing exception error to log (converting invalid strings to 0).
      case util.Success(x) =>
        (x map {
          asInt(_).openOr(0)
        }).toSet
      case util.Failure(ex) =>
        logger.error(s"consumedProducts failed to load file:$ex") // shout it loud, don't swallow error.
        Set.empty[Int]
    }
  }

  /**
    *
    * persists product as part of a set with one element per line in a file.
    * reads full file first to capture existing set (recover "state" temporarily)
    * @throws FileNotFoundException
    * @throws IOException
    * @param product an item in LCBO catalogue with only the attributes we care about so far (much fewer)
    */
  def persist(product: Product): Try[(String, Long)] = {
    /**
      *
      * persistProducts is mutable only to extent of changing state on disk. Alternative solution would be real DB.
      * @param products the set of product ids to store in its entirety.
      * @return writes product ids (input) to configured file.
      */
    def persistProducts(products: Set[Int]): Try[Unit] = {
      import java.io.PrintWriter
      Try {
        val writer = new PrintWriter(new java.io.File(ConsumedProductsFile))
        products foreach { p => writer.write(p.toString + "\n") }
        writer.close()
      }
    }

    Try {
      persistProducts(consumedProducts + product.id) // persists consumedProducts set augmented by new element if we could reconcile it with LCBO inventory
      // otherwise a no-op.
      ("Dummy User", 1.toLong) // semantics have changed after we started to implement PersistProductIDAsDB, which got richer. This is not real code actually.
      // This was merely excluding.
    }
  }
}
