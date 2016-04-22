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
  type JSitemsExtractor[T] = JValue => Iterable[T]
  type GotEnough_? = (Int) => Boolean
  val neverEnough: GotEnough_? = { x => false }

  trait LCBOPageFetcher {
    def collectItemsAsWebClient[T](webApiRoute: String,
                                   xt: JSitemsExtractor[T],
                                   maxPerPage: Int,
                                   params: Seq[(String, Any)] = Seq())
                                  (implicit enough: GotEnough_? = neverEnough): IndexedSeq[T]
  }
}

trait LCBOPageLoader {
  this: LCBOPageFetcherComponent =>

  def collectItemsAsWebClient[T](webApiRoute: String,
                                 xt: JSitemsExtractor[T],
                                 maxPerPage: Int,
                                 params: Seq[(String, Any)] = Seq())
                                (implicit enough: GotEnough_? = neverEnough): IndexedSeq[T] =
    fetcher.collectItemsAsWebClient(webApiRoute, xt, maxPerPage, params)(enough)
}

trait LCBOPageFetcherComponentImpl extends LCBOPageFetcherComponent {
  def fetcher = new FetcherImpl

  case class ItemsOnAPageParams[T](xt: JSitemsExtractor[T], urlRoot: String, maxPerPage: Int, params: Seq[(String, Any)], enough: GotEnough_? )
  // this whole class is hidden from clients. So, who needs to worry about private, public, protected here? No one.
  class FetcherImpl extends LCBOPageFetcher with RestClient {
    val LcboDomainURL = Props.get("lcboDomainURL", "http://") // set it!
    // to present a cleaner interface than the tail recursive method
    def collectItemsAsWebClient[T](webApiRoute: String,
                                   xt: JSitemsExtractor[T],
                                   maxPerPage: Int,
                                   params: Seq[(String, Any)] = Seq())
                                   (implicit enough: GotEnough_? = neverEnough): IndexedSeq[T] = {
      val itemsOnAPageParams = ItemsOnAPageParams[T](xt, LcboDomainURL + webApiRoute, maxPerPage, params, enough )
      // union of this page with next page when we are asked for a full sample
      collectItemsOnAPage( IndexedSeq[T](), 1, itemsOnAPageParams)
    }

    implicit val formats = net.liftweb.json.DefaultFormats

    def buildUrlWithPaging(urlRoot: String, pageNo: Int, maxPerPage: Int, params: (String, Any)*) = {
      val fullParams = params ++ Seq(("per_page", maxPerPage), ("page", pageNo)) // get as many as possible on a page because we could have few matches.
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

    /**
      * LCBO client JSON query handler.
      *
      * @see https://github.com/lift/lift/tree/master/framework/lift-base/lift-json/
      *      Uses tail recursion.
      * @param accumItems    accumulator to facilitate tail recursion
      * @param params        a wrapper of all parameter data we need (see case Class)
      * @return an indexed sequence of product items matching the query and size constraint.
      * @throws java.net.SocketTimeoutException            timeout is reached, slow connection
      * @throws java.io.IOException                        I/O issue
      * @throws net.liftweb.json.JsonParser.ParseException parse problem
      * @throws net.liftweb.json.MappingException          our case class does not match JSon object from API
      *
      */
    @throws(classOf[net.liftweb.json.MappingException])
    @throws(classOf[net.liftweb.json.JsonParser.ParseException])
    @throws(classOf[java.io.IOException])
    @throws(classOf[java.net.SocketTimeoutException])
    @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
    @throws(classOf[TruncatedChunkException]) // that's a brutal one.
    @tailrec
    final def collectItemsOnAPage[T](accumItems: IndexedSeq[T],
                                     currPage: Int,
                                     params: ItemsOnAPageParams[T]): IndexedSeq[T] = {

      val uri = buildUrlWithPaging(params.urlRoot, currPage, params.maxPerPage, params.params: _*)
      val jsonRoot = parse(get(uri)) // fyi: throws ParseException, SocketTimeoutException, IOException,TruncatedChunkException or SocketTimeoutException. Will we survive this?
      val items = params.xt(jsonRoot \ "result") // Uses XPath-like querying to extract data from parsed object jsObj. Throws MappingException
      val revisedAccumItems = accumItems ++ items

      if (isFinalPage(jsonRoot, currPage) || params.enough(items.size + accumItems.size)) {
        logger.info(uri) // log only last one to be less verbose
        return revisedAccumItems
      }
      val revisedParams = ItemsOnAPageParams[T](params.xt, params.urlRoot, params.maxPerPage, params.params, params.enough )
      // union of this page with next page when we are asked for a full sample
      collectItemsOnAPage(revisedAccumItems, currPage +1, revisedParams)
    }
  }

}
