package code.snippet

import net.liftweb._
import net.liftweb.util.CssSel
import util.Helpers._
import common._
import http._
import sitemap._


object Param {
  // Create a menu for /param/somedata
  val menu = Menu.param[ParamInfo]("Param", "Param",
    s => Full(ParamInfo(s)),
    pi => pi.theParam) / "param"
  lazy val loc = menu.toLoc

  def render: CssSel = {
  println(s"in Param.render! $loc")
  "*" #> "Hello" // crashes loc.currentValue.map(_.theParam)
  }
}

