package code.model.pageFetcher

import java.net.URLEncoder

import code.Rest.RestClient
import net.liftweb.json._
import net.liftweb.util.Props
import org.apache.http.TruncatedChunkException

import scala.annotation.tailrec
import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-03-30.
  */
trait LCBOPageFetcher extends RestClient {
  def MaxPerPage: Int

  type JSitemsExtractor[T] = JValue => Iterable[T]

  final def LcboDomainURL = Props.get("lcboDomainURL", "http://") // set it!

  implicit val formats = net.liftweb.json.DefaultFormats

  final def buildUrlWithPaging(urlRoot: String, pageNo: Int, maxPerPage: Int, params: (String, Any)* ): String = {
    val fullParams = params ++ Seq(("per_page", maxPerPage), ("page", pageNo)) // get as many as possible on a page because we could have few matches.
    buildUrl(urlRoot, fullParams)
  }

  final def isFinalPage(jsonRoot: JValue, pageNo: Int): Boolean = {
    //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)
    isFinalPage || totalPages < pageNo + 1
  }

  final def buildUrl(urlRoot: String, params: Seq[(String, Any)] ): String = {
    val encoding = "UTF-8"
    val paramsAsSuffix = params.map(v =>
      URLEncoder.encode(v._1, encoding) +
      "=" +
      URLEncoder.encode(v._2.toString, encoding)
    ).mkString("&")
    if (paramsAsSuffix.nonEmpty) urlRoot + "?" + paramsAsSuffix
    else urlRoot
  }
  /**
    * LCBO client JSON query handler.
    *
    * @see https://github.com/lift/lift/tree/master/framework/lift-base/lift-json/
    *      Uses tail recursion.
    * @param accumItems accumulator to facilitate tail recursion
    * @param urlRoot a LCBO product query without the details of paging, which we handle here
    * @param sizeFulfilled a boolean function indicating we have retrieved enough data, defaults to false, meaning exhaust all data unconditionally
    * @param f filter on the source data as to what we want to retain on a per item basis
    * @param params a collection of key value pairs to build the required uri, excluding paging considerations.
    * @param pageNo client calls this with value 1 (initial page), recursion increments it, designates the pageno for LCBO JSON data when data fits on several pages
    * @return an indexed sequence of product items matching the query and size constraint.
    * @throws java.net.SocketTimeoutException timeout is reached, slow connection
    * @throws java.io.IOException I/O issue
    * @throws net.liftweb.json.JsonParser.ParseException parse problem
    * @throws net.liftweb.json.MappingException our case class does not match JSon object from API
    *
    */
  @throws(classOf[net.liftweb.json.MappingException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @throws(classOf[TruncatedChunkException])  // that's a brutal one.
  @tailrec
  final private def collectItemsOnAPage[T](accumItems: IndexedSeq[T],
                                        extractor:  JSitemsExtractor[T],
                                        urlRoot: String,
                                        pageNo: Int,
                                        params: Seq[(String, Any)],
                                        sizeFulfilled: GotEnough_? = neverEnough): IndexedSeq[T] = {

    val uri = buildUrlWithPaging(urlRoot, pageNo, MaxPerPage, params:_*)
    val jsonRoot = parse(get(uri)) // fyi: throws ParseException, SocketTimeoutException, IOException,TruncatedChunkException or SocketTimeoutException. Will we survive this?
    val items = extractor( jsonRoot \ "result") // Uses XPath-like querying to extract data from parsed object jsObj. Throws MappingException
    val revisedAccumItems = accumItems ++ items

    if (isFinalPage(jsonRoot, pageNo) || sizeFulfilled(items.size + accumItems.size)) {
      logger.info(uri) // log only last one to be less verbose
      return revisedAccumItems
    }
    collectItemsOnAPage(
      revisedAccumItems, // union of this page with next page when we are asked for a full sample
      extractor,
      urlRoot,
      pageNo + 1,
      params,
      sizeFulfilled)
  }

  // to present a cleaner interface than the tail recursive method
  final def collectItemsAsWebClient[T](urlRoot: String,
                                    extractor:  JSitemsExtractor[T],
                                    params: Seq[(String, Any)] = Seq())
                                    (implicit sizeFulfilled: GotEnough_? = neverEnough): IndexedSeq[T] = {
    collectItemsOnAPage(
      IndexedSeq[T](), // union of this page with next page when we are asked for a full sample
      extractor,
      urlRoot,
      1,
      params,
      sizeFulfilled)
  }
}


