package code.model

import net.liftweb.common.{Loggable, Box, Empty}

/**
  * Created by philippederome on 2015-12-13.
  */
case class ProductMessage(product: Box[Product]= Empty) extends Loggable {}
