package code.Rest

import java.net.URI
import skinny.http._

/**
  * Created by philippederome on 2015-12-19.
  */
trait RestClient {
  /**
    * Returns the text (content) from a REST URI as a String.
    * @param uri A complete uri, such as "http://foo.com/bar?param1=val1&param2=val2"
    */
  def get(uri: URI): String =
    HTTP.get(uri.toString).asString
}
