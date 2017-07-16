package code.snippet

import net.liftweb.util.Props
import scala.xml.NodeSeq

/**
  * RadioElements function like radio buttons, one element selected among a list but here we erase the radio buttons and
  * replace normal text labels with image elements. We also handle a frame around the image with a CSS style to suggest selection
  * is active. This RadioElements also contains a title to provide an intuitive tooltip on the image that acts as a button.
  * The name of the RadioElements is what we use to receive the input from the client.
  */
case class RadioElements(name: String, img: NodeSeq)

object RadioElements {
  val thickBorder = Props.get("radio.selectClass", "thickBorder")
  val thinBorder = Props.get("radio.unselectClass", "thinBorder")

  def selectOption(s: String, img: String): RadioElements =
    RadioElements(s, <img name={s} title={s} class={thickBorder} src={img}/>)  // maybe adding button could work?

  def radioOptions(names: Seq[String],
                   defaultName: String,
                   toImg: String => String): Seq[RadioElements] = {
    val getBorderByName: String => (String, String) = name =>
      (name, if (name == `defaultName`) thickBorder else thinBorder)

    val getRadioElements: ((String, String)) => RadioElements = nameAndBorder => {
      val (s, border) = nameAndBorder
      RadioElements(s, <img name={s} title={s} src={toImg(s)} class={border}/>) // selected with style that frames it
    }
    names.map { getBorderByName andThen getRadioElements }
  }
}
