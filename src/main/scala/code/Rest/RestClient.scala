package code.Rest

import skinny.http.{HTTP, HTTPException, Request}
import cats.implicits._

case class HttpTimeouts(sockTimeOut: Int, connTimeOut: Int)

object RestClient {
  val HTTP_PROTOCOL_GOOD_STAT_LOW = 200
  val HTTP_PROTOCOL_GOOD_STAT_HI = 300
  /**
    * Returns the text (content) from a REST URI as a String.
    *
    * @param uri A uri with full path and protocol but without query parameters, such as "http://foo.com/bar
    * @param timeouts to control socket time out or connection time out.
    * @param params a sequence of value pairs that will become ?param1=val1&param2=val2...&paramn=valn"
    * @throws HTTPException
    * @return a pair of JSON String obtained from REST site and the uri we just built or a HTTPException on failure
    */
  def get(uri: String, timeouts: HttpTimeouts, params: (String, Any)*): Either[Throwable, (String, String)] = {
    val r = Request(uri).queryParams(params: _*).
      connectTimeoutMillis(timeouts.connTimeOut).
      readTimeoutMillis(timeouts.sockTimeOut)
    Either.catchNonFatal (HTTP.get(r)) match {
      case Left(t) => Left(t)
      case Right(response) =>
        if (response.status >= HTTP_PROTOCOL_GOOD_STAT_LOW && response.status < HTTP_PROTOCOL_GOOD_STAT_HI) {
          Right(response.asString, r.toString)
        } else {
          Left(new HTTPException(Some("Unexpected response "), response))
        }
    }

  }
}
