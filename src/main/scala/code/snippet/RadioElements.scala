package code.snippet

import net.liftweb.http.SHtml.{ChoiceHolder, ChoiceItem}
import scala.xml.NodeSeq

/**
  * Created by philippederome on 2015-12-25.
  */

case class RadioElements(name: String, img: NodeSeq) {}
object LabelStyle {
  def htmlize(item: ChoiceItem[RadioElements]): NodeSeq = {
    val ns: NodeSeq = item.xhtml ++ item.key.img
    <label class="radio">
      {ns}{item.key.name}
    </label>
  }

  def toForm(holder: ChoiceHolder[RadioElements]): NodeSeq = {
    holder.items.flatMap(htmlize)
  }
}