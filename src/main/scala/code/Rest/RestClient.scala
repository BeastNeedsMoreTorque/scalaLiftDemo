package code.Rest

import java.io.IOException
import java.net.URI

import net.liftweb.common.Loggable
import net.liftweb.util.Props
import org.apache.http.{HttpResponse, HttpStatus, TruncatedChunkException}
import org.apache.http.client._
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import org.apache.http.util.EntityUtils
import org.apache.http.client.ClientProtocolException
import org.apache.http.client.config.RequestConfig

/**
  * Created by philippederome on 2015-12-19.
  */
trait RestClient extends Loggable {
  val sockTimeOut = Props.getInt("http.ClientReadTimeOut", 0)
  val connTimeOut = Props.getInt("http.ClientConnTimeOut", 0)
  val HTTP_PROTOCOL_GOOD_STAT_LOW = HttpStatus.SC_OK
  val HTTP_PROTOCOL_GOOD_STAT_HI = HttpStatus.SC_MULTIPLE_CHOICES

  case class TimeOuts(socketTimeOut: Int, connectionTimeOut: Int)

  /**
    * Returns the text (content) from a REST URI as a String.
    * Returns a blank String if there was a problem.
    * This function will also throw exceptions if there are problems trying
    * to connect to the uri.
    *
    * @param uri A complete uri, such as "http://foo.com/bar?param1=val1&param2=val2"
    */
  @throws(classOf[ClientProtocolException])
  @throws(classOf[IOException])
  @throws(classOf[TruncatedChunkException])// say LCBO sends less data than they promise by chopping off data, a distinct possibility!
  def get(uri: URI, times: TimeOuts = TimeOuts(sockTimeOut, connTimeOut)): String = {
    val httpclient = HttpClients.createDefault()
    logger.trace(s"RestClient.get $uri")
    val httpget = new HttpGet(uri)
    try {
      // Create a custom response handler
      val responseHandler = new ResponseHandler[String]() {
        @Override
        def handleResponse(response: HttpResponse) = {
          val statusLine = response.getStatusLine
          val code  = statusLine.getStatusCode
          if (code >= HTTP_PROTOCOL_GOOD_STAT_LOW && code < HTTP_PROTOCOL_GOOD_STAT_HI) {
            val e = response.getEntity
            if (e ne null) EntityUtils.toString(e) else ""
          } else {
            throw new ClientProtocolException("Unexpected response statusLine: " + statusLine.toString)
          }
        }
      }
      val requestConfig = RequestConfig.custom()
        .setSocketTimeout(times.socketTimeOut)
        .setConnectTimeout(times.connectionTimeOut)
        .build()
      httpget.setConfig(requestConfig)
      httpclient.execute(httpget, responseHandler)
    } finally {
      httpclient.close()
    }
  }
}
