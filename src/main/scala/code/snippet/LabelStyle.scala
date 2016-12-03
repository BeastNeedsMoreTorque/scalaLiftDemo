package code.snippet

import net.liftweb.http.SHtml.{ChoiceHolder, ChoiceItem}
import scala.xml.NodeSeq

object LabelStyle {
  def htmlize(item: ChoiceItem[RadioElements]): NodeSeq =
    <label class="radio">
      {item.xhtml ++ item.key.img}
    </label>

  def toForm(holder: ChoiceHolder[RadioElements]): NodeSeq =
    holder.items.flatMap(htmlize)
}
