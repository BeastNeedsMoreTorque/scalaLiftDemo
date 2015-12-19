package code.snippet

import net.liftweb._
import util.Helpers._

/**
  * Created by philippederome on 2015-12-17.
  */

// a snippet that takes the page parameter information
class ShowParam(pi: ParamInfo) {
  def render = {
    println(s"in ShowParam.render $pi!")
    "*" #> pi.theParam
  }
}
