package code.model

import code.Rest.pagerRestClient
import net.liftweb.util.Props
import net.liftweb.json.DefaultFormats
import net.liftweb.common.{Loggable, Box}
import net.liftweb.json.JsonParser.parse
import scala.util.Try

/**
  * @author Phil Derome
  */
class StoreProvider extends pagerRestClient with Loggable {
  implicit val formats = DefaultFormats

  override def MaxPerPage = Props.getInt("store.lcboMaxPerPage", 0)

  override def MinPerPage = Props.getInt("store.lcboMinPerPage", 0)

  def MaxSampleSize = Props.getInt("store.maxSampleSize", 0)

  def findStore(geo: GeoCoordinates): Try[Box[Store]] = {
    val url = s"$LcboDomainURL/stores?where_not=is_dead" +
      additionalParam("lat", geo.lat) +
      additionalParam("lon", geo.lon)
    Try {
      collectStoresOnAPage(List[Store](), url, MaxSampleSize, pageNo = 1).headOption
    }
  }

  @throws(classOf[net.liftweb.json.MappingException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @scala.annotation.tailrec
  final def collectStoresOnAPage(accumItems: List[Store],
                                 urlRoot: String,
                                 requiredSize: Int,
                                 pageNo: Int): List[Store] = {
    val pageSize = math.max(MinPerPage,
      math.min(MaxPerPage, requiredSize)) // constrained between minPerPage and maxPerPage.
    // specify the URI for the LCBO api url for liquor selection
    val uri = urlRoot + additionalParam("per_page", pageSize) + additionalParam("page", pageNo)
    logger.info(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = for (p <- itemNodes) yield p.extract[Store]
    lazy val outstandingSize = requiredSize - items.size
    lazy val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extract[Boolean]

    if (items.isEmpty || outstandingSize <= 0 || isFinalPage) return accumItems ++ items

    collectStoresOnAPage(
      accumItems ++ items,
      urlRoot,
      outstandingSize,
      pageNo + 1) // union of this page with next page when we are asked for a full sample
  }
}

