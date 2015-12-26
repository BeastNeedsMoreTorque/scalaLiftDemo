package code.snippet

import net.liftweb.http.SHtml.{ChoiceHolder, ChoiceItem}
import net.liftweb.util.Props
import scala.xml.NodeSeq

/**
  * Created by philippederome on 2015-12-25.
  */
case class RadioElements(name: String, img: NodeSeq) {}

object RadioElements {
  val styleForSelectedRadio = Props.get("radioButtonsSelectStyle", "")
  def defaultOption(defaultName: String, DOMId: String, toImg: (String) => String) =
    RadioElements(defaultName, <img id={DOMId} style={styleForSelectedRadio} src={toImg(defaultName)}/>)  // maybe adding button could work?

  def radioOptions(it: Seq[String], defaultName : String, DOMId: String, toImg: (String) => String): Seq[RadioElements] = it.map { (s: String) =>
    if (s == defaultName)
      RadioElements.defaultOption(defaultName, DOMId, toImg )  // selected with style that frames it
    else
      RadioElements (s, <img id={s + "Img"} src={toImg(s)}/>) // not selected, no added style
  }
}

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