package code.model.pageFetcher

import net.liftweb.json.{JNothing, JValue, parseOpt}
import net.liftweb.common.Loggable
import net.liftweb.util.Props
import scala.annotation.tailrec
import scala.collection.IndexedSeq
import cats.implicits._
import skinny.http.{HTTP, HTTPException, Request}

trait LCBOPageFetcher extends Loggable {
  import LCBOPageFetcher._
  type JSitemsExtractor[T] = JValue => IndexedSeq[T]
  type GotEnough_? = (Int) => Boolean
  type ValidateItems[T] = Either[Throwable, IndexedSeq[T]]
  val neverEnough: GotEnough_? = { x => false }

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
    implicit val formats = net.liftweb.json.DefaultFormats

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
    implicit val formats = net.liftweb.json.DefaultFormats
    // LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)
    isFinalPage || totalPages < pageNo + 1
  }

  /**
    * Returns the text (content) from a REST URI as a String.
    *
    * @param uri A uri with full path and protocol but without query parameters, such as "http://foo.com/bar
    * @param params a sequence of value pairs that will become ?param1=val1&param2=val2...&paramn=valn"
    * @throws HTTPException
    * @return a pair of JSON String obtained from REST site and the uri we just built
    */
  def get(uri: String, params: (String, Any)*): (String, String) = {
    val r = Request(uri).queryParams(params: _*).
      connectTimeoutMillis(connTimeOut).
      readTimeoutMillis(sockTimeOut)
    val response = HTTP.get(r)
    if (response.status >= HTTP_PROTOCOL_GOOD_STAT_LOW && response.status < HTTP_PROTOCOL_GOOD_STAT_HI) {
      (response.asString, r.toString)
    } else {
      throw new HTTPException(Some("Unexpected response "), response)
    }
  }
}

object LCBOPageFetcher {
  val LcboDomain: String = Props.get("lcboDomain", "")  // set it!
  val HTTP_PROTOCOL_GOOD_STAT_LOW = 200
  val HTTP_PROTOCOL_GOOD_STAT_HI = 300
  val sockTimeOut = Props.getInt("http.ClientReadTimeOut", 0)
  val connTimeOut = Props.getInt("http.ClientConnTimeOut", 0)
}
