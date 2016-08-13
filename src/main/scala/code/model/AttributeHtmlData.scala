package code.model

/**
  * Created by philippederome on 2016-04-01.
  * AttributeHtmlData: container of data elements to be rendered in a row of a table in DOM for some HTML element such as store or product
  * @param key key for an attribute, a label such as price or name, common across many different items (think of schema)
  * @param value the value of an item for that attribute (such as $10.00, Molson Canadian, or 1 King Street W)
  * @param css a CSS marker affecting how to represent the attribute to be assigned to a html class
  * @param name the html name of the attribute to enable possible DOM manipulation to be assigned to a html name
  */
case class AttributeHtmlData(key: String, value: String, css: String="prodAttrContent", name: String="")
