package code.Rest

import skinny.http._

/**
  * Created by philippederome on 2015-12-19.
  */
trait RestClient {
  /**
    * Returns the text (content) from a REST URI as a String.
    *
    * @param uri A complete uri, such as "http://foo.com/bar
    * @param params a sequence of value pairs that will become ?param1=val1&param2=val2...&paramn=valn"
    * @return a pair of JSon String obtained from REST site and the uri we just built
    */
  def get(uri: String, params: (String, Any)*): (String, String) = {
    val r: Request = Request(uri).queryParams(params: _*)
    (HTTP.get(r).asString, r.toString)
  }
}
