package code.model.pageFetcher

import java.net.URLEncoder
import scala.annotation.tailrec
import scala.collection.IndexedSeq
import net.liftweb.json._
import net.liftweb.util.Props
import org.apache.http.TruncatedChunkException
import code.Rest.RestClient

/**
  * Created by philippederome on 2016-03-30.
  * An excuse to exercise myself in Cake Pattern as per example found at
 *
  * @see http://blog.originate.com/blog/2013/10/21/reader-monad-for-dependency-injection/
  *      He makes very interesting observations about using Reader Monads in web app and cake pattern at outer edges.
  *
  *      Nice personal observation: after having implemented this, the RestClient trait (based on Apache Commons) is buried down at a lower level of abstraction further hidden away.
  *      In previous versions of code, the RestClient trait made it all the way to Product and Inventory.
  *
  *      With cake, interface is more explicit, no need to sort out public, private and we don't need
  *      to think about access rights!
  */
trait LCBOPageFetcherComponent  {
  def fetcher: LCBOPageFetcher
  type JSitemsExtractor[T] = JValue => IndexedSeq[T]
  type GotEnough_? = (Int) => Boolean
  val neverEnough: GotEnough_? = { x => false }

  trait LCBOPageFetcher {
    def collectItemsAsWebClient[T](webApiRoute: String,
                                   xt: JSitemsExtractor[T],
                                   params: Seq[(String, Any)] = Seq())
                                  (implicit enough: GotEnough_? = neverEnough): IndexedSeq[T]
  }
}

trait LCBOPageLoader {
  this: LCBOPageFetcherComponent =>

  def collectItemsAsWebClient[T](webApiRoute: String,
                                 xt: JSitemsExtractor[T],
                                 params: Seq[(String, Any)] = Seq())
                                (implicit enough: GotEnough_? = neverEnough): IndexedSeq[T] =
    fetcher.collectItemsAsWebClient(webApiRoute, xt, params)(enough)
}

trait LCBOPageFetcherComponentImpl extends LCBOPageFetcherComponent {
  def fetcher = new FetcherImpl

  // this whole class is hidden from clients. So, who needs to worry about private, public, protected here? No one.
  class FetcherImpl extends LCBOPageFetcher with RestClient {
    val LcboDomainURL = Props.get("lcboDomainURL", "http://")  // set it!

    /**
      * LCBO client JSON query handler. Exists to present a cleaner interface than the tail recursive method
      *
      * @see https://github.com/lift/lift/tree/master/framework/lift-base/lift-json/
      *      Uses tail recursion.
      * @param accumItems accumulator to facilitate tail recursion
      * @param params     a wrapper of all parameter data we need (see case Class)
      * @return an indexed sequence of product items matching the query and size constraint.
      * @throws java.net.SocketTimeoutException            timeout is reached, slow connection
      * @throws java.io.IOException                        I/O issue
      * @throws net.liftweb.json.JsonParser.ParseException parse problem
      * @throws net.liftweb.json.MappingException          our case class does not match JSon object from API
      * @throws java.net.UnknownHostException
      * @throws TruncatedChunkException  // that's a brutal one, essentially unrecoverable.
      *
      */
    @throws(classOf[net.liftweb.json.MappingException])
    @throws(classOf[net.liftweb.json.JsonParser.ParseException])
    @throws(classOf[java.io.IOException])
    @throws(classOf[java.net.SocketTimeoutException])
    @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
    @throws(classOf[TruncatedChunkException]) // that's a brutal one.
    def collectItemsAsWebClient[T](webApiRoute: String,
                                   xt: JSitemsExtractor[T],
                                   params: Seq[(String, Any)] = Seq())
                                  (implicit enough: GotEnough_? = neverEnough): IndexedSeq[T] = {
      val urlRoot = LcboDomainURL + webApiRoute
      // "go" is an idiom to use tailrec in Functional Programming in Scala as a helper function (and likewise using "closure" as is often found in JS).
      // Function would be pure if we'd bother to pass explicitly as params urlRoot, webApiRoute, xt, params, and enough, but conceptually it's the same. It has no side effect for sure, other than helpful log.
      @tailrec // in general it's good to make recursion tailrec to avoid stack overflow.
      def go(accumItems: IndexedSeq[T], currPage: Int): IndexedSeq[T] = {
        val uri = buildUrlWithPaging(urlRoot, currPage, params: _*)
        val jsonRoot = parse(get(uri)) // fyi: throws ParseException, SocketTimeoutException, IOException,TruncatedChunkException or SocketTimeoutException. Will we survive this?
        val revisedItems = accumItems ++ xt(jsonRoot \ "result") // Uses XPath-like querying to extract data from parsed object jsObj. Throws MappingException
        if (isFinalPage(jsonRoot, currPage) || enough(revisedItems.size)) {
          logger.info(uri) // log only last one to be less verbose
          revisedItems
        }
        else go(revisedItems, currPage + 1)
      }

      go( IndexedSeq(), 1) // tail recursion with classic accumulator as first parameter
    }

    implicit val formats = net.liftweb.json.DefaultFormats

    def buildUrlWithPaging(urlRoot: String, pageNo: Int, params: (String, Any)*) = {
      val fullParams = params ++ Seq(("page", pageNo)) // get as many as possible on a page because we could have few matches.
      buildUrl(urlRoot, fullParams)
    }

    def isFinalPage(jsonRoot: JValue, pageNo: Int) = {
      //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
      val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
      val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)
      isFinalPage || totalPages < pageNo + 1
    }

    def buildUrl(urlRoot: String, params: Seq[(String, Any)]) = {
      val encoding = "UTF-8"
      val paramsAsSuffix = params.map(param =>
        URLEncoder.encode(param._1, encoding) +
          "=" +
          URLEncoder.encode(param._2.toString, encoding)
      ).mkString("&")
      if (paramsAsSuffix.nonEmpty) urlRoot + "?" + paramsAsSuffix
      else urlRoot
    }

  }
}
