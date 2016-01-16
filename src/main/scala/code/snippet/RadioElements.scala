package code.snippet

import net.liftweb.http.SHtml.{ChoiceHolder, ChoiceItem}
import net.liftweb.util.Props
import scala.xml.NodeSeq

/**
  * Created by philippederome on 2015-12-25.
  * RadioElements function like radio buttons, one element selected among a list but here we erase the radio buttons and
  * replace normal text labels with image elements. We also handle a frame around the image with a CSS style to suggest selection
  * is active. This RadioElements also contains a title to provide an intuitive tooltip on the image that acts as a button.
  * The name of the RadioElements is what we use to receive the input from the client.
  */
case class RadioElements(name: String, img: NodeSeq) {}

object RadioElements {
  val styleForSelectedRadio = Props.get("radio.selectClass", "selectframe")
  val styleForUnSelectedRadio = Props.get("radio.unselectClass", "unselectframe")

  def selectOption(s: String, img: String) =
    RadioElements(s, <img name={s} title={s} class={styleForSelectedRadio} src={img}/>)  // maybe adding button could work?

  def radioOptions(it: Seq[String], defaultName: String, toImg: (String) => String): Seq[RadioElements] = it.map { (s: String) =>
    RadioElements(s, <img name={s} title={s} src={toImg(s)} class={if (s == defaultName) styleForSelectedRadio else styleForUnSelectedRadio} />) // selected with style that frames it
  }
}

object LabelStyle {
  def htmlize(item: ChoiceItem[RadioElements]): NodeSeq = {
    <label class="radio">
      {item.xhtml ++ item.key.img}
    </label>
  }

  def toForm(holder: ChoiceHolder[RadioElements]): NodeSeq = {
    holder.items.flatMap(htmlize)
  }
}