package code.Rest

/**
  * Created by philippederome on 2015-12-20.
  */
  object DotDecimalString {  // extractor for below
    def apply(s: String): String = s
    def unapply(s: String): Option[String] = if (s.contains(',')) Some(s.replace(',', '.'))  else Some(s + ".0")
  }
