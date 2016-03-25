package code.Rest

import org.apache.http.HttpEntity
import org.apache.http.client._
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet}
import org.apache.http.impl.client.{CloseableHttpClient, HttpClients}
import org.apache.http.util.EntityUtils

/**
  * Created by philippederome on 2015-12-19. First implementation had been from Alvin Alexander: http://alvinalexander.com/scala/how-to-write-scala-http-get-request-client-source-fromurl
  * With errors of data truncation, I settled for Apache Commons on 2016-03-25.
  */
trait RestClient {

  /**
    * Returns the text (content) from a REST URL as a String.
    * Returns a blank String if there was a problem.
    * This function will also throw exceptions if there are problems trying
    * to connect to the url.
    *
    * @param url A complete URL, such as "http://foo.com/bar"
    */
  def get(url: String): String = {

    val httpclient: CloseableHttpClient = HttpClients.createDefault()
    val httpGet: HttpGet = new HttpGet(url)
    val response: CloseableHttpResponse = httpclient.execute(httpGet)
    // The underlying HTTP connection is still held by the response object
    // to allow the response content to be streamed directly from the network socket.
    // In order to ensure correct deallocation of system resources
    // the user MUST call CloseableHttpResponse#close() from a finally clause.
    // Please note that if response content is not fully consumed the underlying
    // connection cannot be safely re-used and will be shut down and discarded
    // by the connection manager.
    try {

      val status = response.getStatusLine().getStatusCode()
      if (status >= 200 && status < 300) {
        val entity: HttpEntity = response.getEntity()
        if (entity != null) EntityUtils.toString(entity)
        else ""
      } else {
        throw new ClientProtocolException("Unexpected response status: " + status)
      }
    } finally {
      response.close()
    }
  }
}