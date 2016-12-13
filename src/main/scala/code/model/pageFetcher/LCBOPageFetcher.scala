package code.model.pageFetcher

import code.Rest.{HttpTimeouts, RestClient}
import code.model.{Store, Product, Inventory, InventoryAsLCBOJson}
import net.liftweb.common.{Full, Loggable}
import net.liftweb.json.{JNothing, JValue, parseOpt}
import net.liftweb.json.JsonAST.{JField, JInt}
import code.model.GlobalLCBO_IDs._
import net.liftweb.util.Props

import scala.annotation.tailrec
import scala.collection.IndexedSeq

trait LCBOPageFetcher {
  import LCBOPageFetcher._
  /**
    * LCBO client JSON query handler. Exists to present a cleaner interface than the tail recursive method
    *
    * @see https://github.com/lift/lift/tree/master/framework/lift-base/lift-json/
    *      Uses tail recursion.
    * @param path the path we use for web client query e.g. http://<hostname>:<port>/<apipath> excluding query parameters
    * @param xt a mechanisms to extract items[T] from JSON
    * @param params     a wrapper of all parameter data we need (see case Class)
    * @return an indexed sequence of product items matching the query and size constraint.
    */
  def collectItemsAsWebClient[T](path: String,
                                 xt: JSitemsExtractor[T],
                                 params: Seq[(String, Any)] = Seq())
                                (implicit enough: GotEnough_? = { x => false }): ValidateItems[T] =
    go( IndexedSeq(), 1, xt, params, enough, s"http://$LcboDomain/$path")
}

object LCBOPageFetcher extends Loggable {
  type ValidateItems[T] = Either[Throwable, IndexedSeq[T]]
  type JSitemsExtractor[T] = JValue => IndexedSeq[T]
  type GotEnough_? = (Int) => Boolean

  val LcboDomain: String = Props.get("lcboDomain", "")  // set it!
  val httpTimeOuts = HttpTimeouts(Props.getInt("http.ClientReadTimeOut", 0), Props.getInt("http.ClientConnTimeOut", 0))

  implicit val formats = net.liftweb.json.DefaultFormats

  // "go" is an idiom to use tailrec in Functional Programming in Scala as a helper function (and likewise using "closure" as is often found in JS).
  // Function would be pure if we'd bother to pass explicitly as params urlRoot, webApiRoute, xt, params, and enough, but conceptually it's the same.
  // It has no side effect for sure, other than helpful log.
  // @throws HTTPException
  @tailrec // in general it's good to make recursion tailrec to avoid stack overflow. Tail recursion with classic accumulator as first parameter
  def go[T](accumItems: IndexedSeq[T],
            currPage: Int,
            xt: JSitemsExtractor[T],
            params: Seq[(String, Any)],
            enough: GotEnough_?,
            uriRoot: String): ValidateItems[T] =
    // get as many as possible on a page because we could have few matches.
    RestClient.get(uriRoot, httpTimeOuts, params ++ Seq(("page", currPage)): _*) match {
    case Left(t) => Left(t)
    case Right((msg, uri)) =>
      val jsonRoot = parseOpt(msg).fold[JValue](JNothing)(identity)
      // fyi: throws plenty of various exceptions.
      val revisedItems = accumItems ++ xt(jsonRoot \ "result")
      if (isFinalPage(jsonRoot, currPage) || enough(revisedItems.size)) {
        logger.info(uri) // log only last one to be less verbose (technically the String for the logger, could be pushed out perhaps in a Log Monad)
        Right(revisedItems)
      }
      else go(revisedItems, currPage + 1, xt, params, enough, uriRoot)
  }

  def isFinalPage(jsonRoot: JValue, pageNo: Int): Boolean = {
    // LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)
    isFinalPage || totalPages < pageNo + 1
  }

  val extractInventory: JSitemsExtractor[Inventory] = { jVal =>
    for {p <- jVal.children.toIndexedSeq
         inv <- p.extractOpt[InventoryAsLCBOJson]
         storeid <- Store.lcboKeyToPKMap.get(inv.store_id.LcboKeyID)
         productid <- Product.lcboKeyToPKMap.get(inv.product_id.LcboKeyID)
         newInv = Inventory(storeid, productid, inv)
    } yield newInv
  }

  /**
    * Some LCBO entities require to back patch JSon read in "id" as a separate column in Record (lcbo_id). They do so with the logic below (idFix = transform).
    * In other words, JSON comes in as id=123 and we need to store that to table.column[lcbo_id]. The crux of problem is Lift Record wanting to use Fields
    * that have a functional read-only interface while accepting to do sets on the columns and that clashes with underlying Squeryl ORM library that has defined
    * id as a def (a true read-only item). And this id thingie is required for the whole MainSchema to work with the ORM relationships in memory.
    */
  def extractStore: JSitemsExtractor[Store] = json => {
    val idFix = json.transformField {
      case JField("id", JInt(n)) => JField("lcbo_id", JInt(n)) // see above paragraph text for justification.
    }
    idFix.children.collect { case(n: JValue) => Store.meta.fromJValue(n) }
      .collect { case(Full(x)) => x }.toIndexedSeq
  }

  val extractProduct: JSitemsExtractor[Product] = json => {
    val idFix = json.transformField {
      case JField("id", JInt(n)) => JField("lcbo_id", JInt(n)) // see above paragraph text for justification.
    }
    idFix.children.collect { case(n: JValue) => Product.meta.fromJValue(n) }
      .collect { case(Full(x)) => x }.toIndexedSeq
  }
}
