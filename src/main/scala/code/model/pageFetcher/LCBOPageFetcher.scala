package code.model.pageFetcher

import code.Rest.RestClient
import net.liftweb.json.{JNothing, JValue, parseOpt}
import net.liftweb.common.Loggable
import net.liftweb.util.Props
import scala.annotation.tailrec
import scala.collection.IndexedSeq
import cats.implicits._
/**
  *
  * @see http://blog.originate.com/blog/2013/10/21/reader-monad-for-dependency-injection/
  *      He makes very interesting observations about using Reader Monads in web app and cake pattern at outer edges.
  *
  */
trait LCBOPageLoader extends RestClient with Loggable {
  type JSitemsExtractor[T] = JValue => IndexedSeq[T]
  type GotEnough_? = (Int) => Boolean
  type ValidateItems[T] = Either[Throwable, IndexedSeq[T]]
  val neverEnough: GotEnough_? = { x => false }

  val LcboDomain = Props.get("lcboDomain", "")  // set it!
  implicit val formats = net.liftweb.json.DefaultFormats
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
                                (implicit enough: GotEnough_? = neverEnough): ValidateItems[T] = Either.catchNonFatal {
    val uriRoot: String = s"http://$LcboDomain/$path"
    // "go" is an idiom to use tailrec in Functional Programming in Scala as a helper function (and likewise using "closure" as is often found in JS).
    // Function would be pure if we'd bother to pass explicitly as params urlRoot, webApiRoute, xt, params, and enough, but conceptually it's the same.
    // It has no side effect for sure, other than helpful log.
    // @throws HTTPException
    @tailrec // in general it's good to make recursion tailrec to avoid stack overflow.
    def go(accumItems: IndexedSeq[T], currPage: Int): IndexedSeq[T] = {
      // get as many as possible on a page because we could have few matches.
      val (msg, uri) = get(uriRoot, params ++ Seq(("page", currPage)): _*)
      val jsonRoot = parseOpt(msg).fold[JValue](JNothing)(identity)
      // fyi: throws plenty of various exceptions.
      val revisedItems = accumItems ++ xt(jsonRoot \ "result") // Uses XPath-like querying to extract data from parsed object jsObj.
      if (isFinalPage(jsonRoot, currPage) || enough(revisedItems.size)) {
        logger.info(uri) // log only last one to be less verbose
        revisedItems
      }
      else go(revisedItems, currPage + 1)
    }
    go( IndexedSeq(), 1) // tail recursion with classic accumulator as first parameter
  }

  def isFinalPage(jsonRoot: JValue, pageNo: Int): Boolean = {
    // LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)
    isFinalPage || totalPages < pageNo + 1
  }
}

