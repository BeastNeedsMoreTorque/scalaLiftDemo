package code
package model

import java.io.IOException
import code.Rest.pagerRestClient
import net.liftweb.common.{Loggable, Box}

import scala.util.{Random, Try}
import net.liftweb.json.JsonParser._
import net.liftweb.json.{DefaultFormats, MappingException}

/**
  * @author Phil Derome
  *         Singleton providing functionality for which liquor products are considered having been consumed.
  *         Thread safety is this class' business. No var (mutable) in this  and dependent parents, other than changing state of file/output/logs.
  *         To generate documentation, type doc in sbt or use Intellij IDEA and request doc (Tools --> Scaladoc).
  */
abstract class ProductProvider extends pagerRestClient with Loggable {
  implicit val formats = DefaultFormats

  def persist(product: Product): Try[(String, Long)] // may throw, so advertise it with a Try

  /**
    * We call up LCBO site each time we get a query with NO caching. This is inefficient but simple and yet reasonably responsive.
    * Select a random product that matches the parameters subject to a max sample size.
    *
    * @param store a String representing a numeric code of a LCBO store
    * @param category a String such as beer, wine, mostly matching primary_category at LCBO, or an asset category.
    * @return
    */
  def recommend(maxSampleSize: Int, store: Int, category: String): Try[Box[Product]] = {
    Try {
      val randomIndex1 = Random.nextInt(math.max(1, maxSampleSize)) // max constraint is defensive for poor client usage (negative numbers).
      val prods = productListByStoreCategory(randomIndex1, store = store, category)
      val randomIndex2 = if (prods.size < randomIndex1) // an exercise in avoiding a var for a single variable randomIdex for FP fans. Arguable that it's needed.
        Random nextInt math.min(randomIndex1, prods.size) // adjusts for inventory being too small
      else
        randomIndex1

      prods.take(randomIndex2 + 1).takeRight(1).headOption // pick up the one dictated by randomIndex (must take at least 1 even if randomIndex is 0)
    }
  }

  /**
    * Purchases a product by increasing user-product count (amount) in database as a way to monitor usage..
    * @param product contains a product
    * @return a Try to force exception reporting further up, capturing how many times user has consumed product.
    */
  def consume(product: Product): Try[(String, Long)] = persist(product) // yeah, could do other things such as real payment transaction and exchange of asset.

  /**
    * LCBO client JSON query handler. So naturally, the code is hard-coded towards the structure of LCBO documents with tokens as is.
    * For Liftweb JSON extraction after parse,
    * @see https://github.com/lift/lift/tree/master/framework/lift-base/lift-json/
    *      don't go to more pages than user implicitly requests via requiredSize that should not be exceeded.
    *      Would Streams collection be handy for paging here? Depends on consumption usage perhaps.
    *      Uses tail recursion (might prevent us from advertizing exceptions with a Try return type?).
    * @param accumItems accumulator to facilitate tail recursion
    * @param urlRoot a LCBO product query without the details of paging, which we handle here
    * @param requiredSize required size of products that are asked for. May get less if there are fewer matches, but will not go above that size.
    * @param pageNo client calls this with value 1 (initial page), recursion increments it, designates the pageno for LCBO JSON data when data fits on several pages
    * @param myFilter client's filter that can be applied as we process the data before mapping/extracting it out to client data.
    *                 In principle, should be faster when user filters reject many values, but no empirical evidence here.
    * @return a vector of product items matching the query and size constraint, though we may go a bit over the size by multiple of page sizes.
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
  @scala.annotation.tailrec
  final def collectItemsOnAPage(accumItems: List[Product],
                                urlRoot: String,
                                requiredSize: Int,
                                pageNo: Int,
                                myFilter: Product => Boolean = { p: Product => true }): List[Product] = {
    val pageSize = math.max(MinPerPage,
      math.min(MaxPerPage, requiredSize)) // constrained between minPerPage and maxPerPage.
    // specify the URI for the LCBO api url for liquor selection
    val uri = urlRoot + additionalParam("per_page", pageSize) + additionalParam("page", pageNo)
    logger.info(uri)
    val pageContent = get(uri, HttpClientConnTimeOut, HttpClientReadTimeOut) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val itemNodes = (jsonRoot \ "result").children // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = (for (p <- itemNodes) yield p.extract[Product]).filter(myFilter)
    lazy val outstandingSize = requiredSize - items.size

    // Collects into our vector of products products the attributes we care about (extract[Product]). Then filter out unwanted data.
    // fyi: throws Mapping exception.
    //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    lazy val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extract[Boolean]

    if (items.isEmpty || outstandingSize <= 0 || isFinalPage) return accumItems ++ items
    // Deem as last page if there are no products found on current page or LCBO tells us it's final page.
    // Similarly, even if we're not on last page and there are more products, having reached our required size.

    // tail recursion enforced.
    collectItemsOnAPage(
      accumItems ++ items,
      urlRoot,
      outstandingSize,
      pageNo + 1,
      myFilter) // union of this page with next page when we are asked for a full sample
  }

  import java.net.SocketTimeoutException

  // for reflection and generating documentation
  /**
    * Queries LCBO matching category and storeId for a sample size as specified by client, with category considered optional, though not tested when optional.
    * Full URL will be built as follows: http://lcbo.com/products?store_id=<storeId>&q=<category.toLowerCase()>&per_page=<perPage>
    * LCBO allows to specify q as query to specify pattern match on product name (e.g. beer, wine)
    * for pattern match LCBO uses lower case but for actual product category it's upper case, so to make comparisons, we will need to account for that
    * primary_category in catalog or p.primary_category so we need a conversion function to adjust)
    * @param requiredSize upper bound on #items we need. Try to match it if possible.
    * @param store id  of Store at LCBO
    * @param category wine, spirits, and so on
    * @return collection of LCBO products while throwing.
    * @throws SocketTimeoutException timeout reached
    * @throws IOException I/O issue
    * @throws ParseException parse issue
    * @throws MappingException, etc
    */
  @throws(classOf[SocketTimeoutException])
  @throws(classOf[IOException])
  @throws(classOf[ParseException])
  @throws(classOf[MappingException])
  def productListByStoreCategory(requiredSize: Int, store: Int, category: String = ""): Vector[Product] = {
    val url = s"$LcboDomainURL/products?store_id=$store" + additionalParam("q", category) // does not handle first one such as storeId, which is artificially mandatory
    val filter = { p: Product => p.primaryCategory == LiquorCategory.toPrimaryCategory(category) &&
      !p.isDiscontinued
    }
    // accommodates for the rather unpleasant different ways of seeing product categories (beer and Beer or coolers and Ready-to-Drink/Coolers

    collectItemsOnAPage(
      List[Product](),
      url,
      requiredSize,
      pageNo = 1,
      filter).take(requiredSize).toVector
  }
}

