package code.Rest

import java.io.IOException
import net.liftweb.common.Loggable
import org.apache.http.{HttpEntity, HttpResponse}
import org.apache.http.client._
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.HttpClients
import org.apache.http.util.EntityUtils
import org.apache.http.client.ClientProtocolException

/**
  * Created by philippederome on 2015-12-19. First implementation had been from Alvin Alexander: http://alvinalexander.com/scala/how-to-write-scala-http-get-request-client-source-fromurl
  * With errors of data truncation, I settled for Apache Commons on 2016-03-25 and simply translated to Scala.
  */
trait RestClient extends Loggable {

  /**
    * Returns the text (content) from a REST URL as a String.
    * Returns a blank String if there was a problem.
    * This function will also throw exceptions if there are problems trying
    * to connect to the url.
    *
    * @param url A complete URL, such as "http://foo.com/bar"
    */
  @throws(classOf[ClientProtocolException])
  @throws(classOf[IOException])  // say LCBO sends less data than they promise by chopping off data, a distinct possibility!
  def get(url: String): String = {

    val httpclient = HttpClients.createDefault()
    try {
      logger.trace(s"RestClient.get $url")
      val httpget = new HttpGet(url)
      // Create a custom response handler
      val responseHandler = new ResponseHandler[String]() {
        @Override
        def handleResponse( response: HttpResponse): String = {
          val status  = response.getStatusLine().getStatusCode()
          if (status >= 200 && status < 300) {
            val entity: HttpEntity  = response.getEntity()
            if (entity != null) EntityUtils.toString(entity)
            else ""
          } else {
            throw new ClientProtocolException("Unexpected response status: " + status)
          }
        }

      }
      httpclient.execute(httpget, responseHandler)
    } finally {
      httpclient.close()
    }
  }
}