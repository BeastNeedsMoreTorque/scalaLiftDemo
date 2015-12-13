package code.snippet


import net.liftweb.util.Helpers._

/**
  * Created by philippederome on 2015-12-12.
  */
object LongTime {

  def end = {
    Thread.sleep(3000)
    now.toString
  }
  def render =
    "#start" #> now.toString &
    "#end" #> end
}
