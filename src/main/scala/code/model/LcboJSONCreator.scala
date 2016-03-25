package code.model

import java.net.URLEncoder

import code.Rest.RestClient
import net.liftweb.json._
import net.liftweb.record.Record

import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer

/**
  * Created by philippederome on 2016-03-23.
  */
trait LcboJSONCreator[T <: LcboJSONCreator[T]] extends Record[T] with RestClient {
  self: T =>

  def setLcboId(id: Long): Unit

  def buildUrl(urlRoot: String, params: (String, Any)* ): String = {
    val encoding = "UTF-8"
    urlRoot + (params.map(v => URLEncoder.encode(v._1, encoding) + "=" + URLEncoder.encode(v._2.toString, encoding)).mkString("&")
    match {case s if s.length == 0 => ""; case s => "?" + s})  // if there are parameters, prepend with ?
  }

  implicit val formats = net.liftweb.json.DefaultFormats

  def extractLcboItems(urlRoot: String, pageNo: Int, maxPerPage: Int, params: (String, Any)*): (IndexedSeq[T], JValue, String) = {
    // specify the URI for the LCBO api url for liquor selection
    // get as many as possible on a page because we could have few matches.
    val fullParams = Seq("per_page" -> maxPerPage, "page" -> pageNo) ++ params
    val uri = buildUrl(urlRoot, fullParams:_* )
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
    (items.toIndexedSeq, jsonRoot, uri)
  }
}

