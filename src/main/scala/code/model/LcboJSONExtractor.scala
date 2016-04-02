package code.model

import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer

import net.liftweb.json._
import net.liftweb.record.Record
import org.apache.http.TruncatedChunkException

/**
  * Created by philippederome on 2016-03-23.
  */
trait LcboJSONExtractor[T <: LcboJSONExtractor[T]] extends Record[T] with LCBOPageFetcher[T] {
  self: T =>

  def setLcboId(id: Long): Unit

  @throws(classOf[net.liftweb.json.MappingException])
  @throws(classOf[net.liftweb.json.JsonParser.ParseException])
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  @throws(classOf[TruncatedChunkException])  // that's a brutal one.
  final def extractItems(uri: String): (IndexedSeq[T], JValue) = {
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

