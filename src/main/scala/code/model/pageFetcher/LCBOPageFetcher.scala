package code.model.pageFetcher

import cats.data.Xor
import code.Rest.RestClient
import net.liftweb.json._
import net.liftweb.common.Loggable
import net.liftweb.util.Props
import scala.annotation.tailrec
import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-03-30.
  * An excuse to exercise myself in Cake Pattern as per example found at
  *
  * @see http://blog.originate.com/blog/2013/10/21/reader-monad-for-dependency-injection/
  *      He makes very interesting observations about using Reader Monads in web app and cake pattern at outer edges.
  *
  *      Nice personal observation: after having implemented this,
  *      the RestClient trait is buried down at a lower level of abstraction further hidden away.
  *      In previous versions of code, the RestClient trait made it all the way to Product and Inventory.
  *
  *      With cake, interface is more explicit, no need to sort out public, private and we don't need
  *      to think about access rights!
  */
trait LCBOPageFetcherComponent  {
  type JSitemsExtractor[T] = JValue => IndexedSeq[T]
  type GotEnough_? = (Int) => Boolean
  type ValidateItems[T] = Xor[Throwable, IndexedSeq[T]]
  val neverEnough: GotEnough_? = { x => false }

  def fetcher: LCBOPageFetcher

  trait LCBOPageFetcher {
    def collectItemsAsWebClient[T](webApiRoute: String,
                                   xt: JSitemsExtractor[T],
                                   params: Seq[(String, Any)] = Seq())
                                  (implicit enough: GotEnough_? = neverEnough): ValidateItems[T]
  }
}

trait LCBOPageLoader {
  this: LCBOPageFetcherComponent =>

  def collectItemsAsWebClient[T](webApiRoute: String,
                                 xt: JSitemsExtractor[T],
                                 params: Seq[(String, Any)] = Seq())
                                (implicit enough: GotEnough_? = neverEnough): ValidateItems[T] =
    fetcher.collectItemsAsWebClient(webApiRoute, xt, params)(enough)
}

trait LCBOPageFetcherComponentImpl extends LCBOPageFetcherComponent with Loggable {
  def fetcher: LCBOPageFetcher = new FetcherImpl

  // this whole class is hidden from clients. So, who needs to worry about private, public, protected here? No one.
  class FetcherImpl extends LCBOPageFetcher with RestClient {
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
                                  (implicit enough: GotEnough_? = neverEnough): ValidateItems[T] = Xor.catchNonFatal {
      val uriRoot: String = s"http://$LcboDomain/$path"
      // "go" is an idiom to use tailrec in Functional Programming in Scala as a helper function (and likewise using "closure" as is often found in JS).
      // Function would be pure if we'd bother to pass explicitly as params urlRoot, webApiRoute, xt, params, and enough, but conceptually it's the same.
      // It has no side effect for sure, other than helpful log.
      // @throws HTTPException
      @tailrec // in general it's good to make recursion tailrec to avoid stack overflow.
      def go(accumItems: IndexedSeq[T], currPage: Int): IndexedSeq[T] = {
        // get as many as possible on a page because we could have few matches.
        val (msg, uri) = get(uriRoot, params ++ Seq(("page", currPage)): _*)
        val jsonRoot = parse(msg)
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
}
