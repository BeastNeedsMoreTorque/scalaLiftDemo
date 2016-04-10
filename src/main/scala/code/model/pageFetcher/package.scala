package code.model

/**
  * Created by philippederome on 2016-04-09.
  */
package object pageFetcher {
  type GotEnough_? = (Int) => Boolean

  val neverEnough: GotEnough_? = { x => false }
}
