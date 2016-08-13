package code.Rest

import net.liftweb.util.Props
import skinny.http.{HTTPException, _}

/**
  * Created by philippederome on 2015-12-19.
  */
trait RestClient {
  val HTTP_PROTOCOL_GOOD_STAT_LOW = 200
  val HTTP_PROTOCOL_GOOD_STAT_HI = 300
  val sockTimeOut = Props.getInt("http.ClientReadTimeOut", 0)
  val connTimeOut = Props.getInt("http.ClientConnTimeOut", 0)
  /**
    * Returns the text (content) from a REST URI as a String.
    *
    * @param uri A uri with full path and protocol but without query parameters, such as "http://foo.com/bar
    * @param params a sequence of value pairs that will become ?param1=val1&param2=val2...&paramn=valn"
    * @throws HTTPException
    * @return a pair of JSon String obtained from REST site and the uri we just built
    */
  def get(uri: String, params: (String, Any)*): (String, String) = {
    val r: Request = Request(uri).queryParams(params: _*).connectTimeoutMillis(connTimeOut).readTimeoutMillis(sockTimeOut)
    val response = HTTP.get(r)
    if (response.status >= HTTP_PROTOCOL_GOOD_STAT_LOW && response.status < HTTP_PROTOCOL_GOOD_STAT_HI) {
      (response.asString, r.toString)
    } else {
      throw new HTTPException(Some("Unexpected response "), response)
    }
  }
}
