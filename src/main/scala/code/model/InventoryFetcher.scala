package code.model

import net.liftweb.json._
import net.liftweb.util.Props

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-03-30.
  */
object InventoryFetcher extends LCBOPageFetcher[InventoryAsLCBOJson] {
  override def MaxPerPage = Props.getInt("inventory.lcboMaxPerPage", 0)

  final def extractItems(uri: String): (IndexedSeq[InventoryAsLCBOJson], JValue) = {
    val pageContent = get(uri)
    val jsonRoot = parse(pageContent)
    val itemNodes = (jsonRoot \ "result").children.toVector // Uses XPath-like querying to extract data from parsed object jsObj.
    val items = (for (p <- itemNodes) yield p.extract[InventoryAsLCBOJson]).filterNot(_.is_dead)
    (items, jsonRoot)
  }
}

