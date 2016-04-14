package code.snippet

import net.liftweb.http.SHtml.{ChoiceHolder, ChoiceItem}

import scala.xml.NodeSeq

/**
  * Created by philippederome on 2016-04-14.
  */
object LabelStyle {
  def htmlize(item: ChoiceItem[RadioElements]): NodeSeq =
    <label class="radio">
      {item.xhtml ++ item.key.img}
    </label>

  def toForm(holder: ChoiceHolder[RadioElements]): NodeSeq =
    holder.items.flatMap(htmlize) // rare usage of flatMap in place of comprehension!

}
