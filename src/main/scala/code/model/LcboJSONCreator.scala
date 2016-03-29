package code.model

import java.net.URLEncoder

import code.Rest.RestClient
import net.liftweb.json._
import net.liftweb.record.Record
import net.liftweb.util.Props

import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer

/**
  * Created by philippederome on 2016-03-23.
  */
trait LcboJSONCreator[T <: LcboJSONCreator[T]] extends Record[T] with RestClient {
  self: T =>

  def setLcboId(id: Long): Unit

  final def buildUrlWithPaging(urlRoot: String, pageNo: Int, maxPerPage: Int, params: (String, Any)* ): String = {
    val fullParams = params ++ Seq(("per_page", maxPerPage), ("page", pageNo)) // get as many as possible on a page because we could have few matches.
    buildUrl(urlRoot, fullParams)
  }

  final def buildUrl(urlRoot: String, params: Seq[(String, Any)] ): String = {
    val encoding = "UTF-8"
    urlRoot + (params.map(v => URLEncoder.encode(v._1, encoding) + "=" + URLEncoder.encode(v._2.toString, encoding)).mkString("&")
    match {case s if s.length == 0 => ""; case s => "?" + s})  // if there are parameters, prepend with ?
  }

  implicit val formats = net.liftweb.json.DefaultFormats

  final def LcboDomainURL = Props.get("lcboDomainURL", "http://") // set it!

  final def isFinalPage(jsonRoot: JValue, pageNo: Int): Boolean = {
    //LCBO tells us it's last page (Uses XPath-like querying to extract data from parsed object).
    val isFinalPage = (jsonRoot \ "pager" \ "is_final_page").extractOrElse[Boolean](false)
    val totalPages = (jsonRoot \ "pager" \ "total_pages").extractOrElse[Int](0)
    isFinalPage || totalPages < pageNo + 1
  }

  final def extractLcboItems(uri: String): (IndexedSeq[T], JValue) = {
    val pageContent = get(uri) // fyi: throws IOException or SocketTimeoutException
    val jsonRoot = parse(pageContent) // fyi: throws ParseException
    val nodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
    // collect our list of products in items and filter out unwanted products

    val items = ArrayBuffer[T]()
    for (p <- nodes;
         key <- (p \ "id").extractOpt[Long];
         rec <- meta.fromJValue(p)) {
      rec.setLcboId(key) //hack. Record is forced to use "id" as read-only def, which means we cannot extract it direct... Because of PK considerations at Squeryl.
      items += rec
    }
    (items.toIndexedSeq, jsonRoot)
  }
}

