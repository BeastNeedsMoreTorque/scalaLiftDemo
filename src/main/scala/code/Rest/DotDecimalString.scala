package code.Rest

/**
  * Created by philippederome on 2015-12-20.
  */
  //case class DotDecimalString(s: String) {
  //  def unapply(s: String): Option[String] = if (s.indexOf(',') >=0) Some(s.replace(',', '.'))  else Some(s + ".0")
  //}

  object DotDecimalString {  // extractor for below
    def apply(s: String): String = s
    def unapply(s: String): Option[String] = if (s.indexOf(',') >=0) Some(s.replace(',', '.'))  else Some(s + ".0")
  }
