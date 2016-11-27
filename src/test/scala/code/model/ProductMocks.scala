package code.model

import code.model.GlobalLCBO_IDs._

import scala.collection.IndexedSeq

/**
  * Created by philippederome on 2016-08-25.
  */
package object productMocks {
  val emptyProducts = Right( IndexedSeq[Product]() )

  trait MockProduct extends IProduct {
    override def isDiscontinued: Boolean = false

    override def imageThumbUrl: String = "http://lcboapi.com/someimage.png"

    override def streamAttributes: IndexedSeq[AttributeHtmlData] =
      (AttributeHtmlData("Name:", Name) ::
        AttributeHtmlData("Primary Category:", primaryCategory) ::
        AttributeHtmlData("Price:", price) ::
        AttributeHtmlData("Alcohol content:", alcoholContent) ::
        Nil).filterNot { attr => attr.value == "null" || attr.value.isEmpty }.toVector

    def identifier: Long

    override def pKey: P_KEY = identifier.PKeyID

    override def lcboKey: LCBO_KEY = identifier.LcboKeyID
  }

  trait MockBeer extends IProduct with MockProduct {
    override def primaryCategory: String = "beer"

    override def price: String = "$2.00"

    override def alcoholContent: String = "5.0%"
  }

  trait MockWine extends IProduct with MockProduct {
    override def primaryCategory: String = "wine"

    override def price: String = "$15.00"

    override def alcoholContent: String = "16.0%"
  }

  object Heineken extends MockBeer {
    override def identifier: Long = 1

    override def Name: String = "Heineken"
  }

  object MillStLager extends MockBeer {
    override def identifier: Long = 2

    override def alcoholContent: String = "5.5%"

    override def Name: String = "Mill Street Lager"

    override def price: String = "$2.50"
  }

  object OysterBay extends MockWine {
    override def identifier: Long = 1000

    override def alcoholContent: String = "16.0%"

    override def Name: String = "Oyster Bay"

    override def price: String = "$21.00"
  }

  object ChampagneKrug extends MockWine {
    override def identifier: Long = 1001

    override def alcoholContent: String = "14.0%"

    override def Name: String = "Krug Champagne"

    override def price: String = "$150.00"
  }

}