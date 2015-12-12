package code.model

import java.net.{HttpURLConnection, URL}

trait RestClient {

  /**
    * @author Alvin Alexander
    *         Returns the text (content) from a REST URL as a String.
    *         Inspired by http://matthewkwong.blogspot.com/2009/09/scala-scalaiosource-fromurl-blo...
    * @see http://alvinalexander.com/blog/post/java/how-open-url-read-contents-http...
    *
    *      The `connectTimeout` and `readTimeout` comes from the Java URLConnection
    *      class Javadoc.
    * @param url The full URL to connect to.
    * @param connectTimeout Sets a specified timeout value, in milliseconds,
    *                       to be used when opening a communications link to the resource referenced
    *                       by this URLConnection. If the timeout expires before the connection can
    *                       be established, a java.net.SocketTimeoutException
    *                       is raised. A timeout of zero is interpreted as an infinite timeout.
    *                       Defaults to 5000 ms.
    * @param readTimeout If the timeout expires before there is data available
    *                    for read, a java.net.SocketTimeoutException is raised. A timeout of zero
    *                    is interpreted as an infinite timeout. Defaults to 5000 ms.
    * @param requestMethod Defaults to "GET". (Other methods have not been tested.)
    * @see http://alvinalexander.com/comment/reply/6774
    * @example get("http://www.example.com/getInfo")
    * @example get("http://www.example.com/getInfo", 5000)
    * @example get("http://www.example.com/getInfo", 5000, 5000)
    * @throws java.io.IOException I/O issue
    * @throws java.net.SocketTimeoutException timeout reached
    * @throws java.net.UnknownHostException perhaps no local network connection

    */
  @throws(classOf[java.io.IOException])
  @throws(classOf[java.net.SocketTimeoutException])
  @throws(classOf[java.net.UnknownHostException]) // no wifi/LAN connection for instance
  def get(url: String,
          connectTimeout: Int = 5000,
          readTimeout: Int = 5000,
          requestMethod: String = "GET"): String = {
    val connection = new URL(url).openConnection.asInstanceOf[HttpURLConnection]
    connection.setConnectTimeout(connectTimeout)
    connection.setReadTimeout(readTimeout)
    connection.setRequestMethod(requestMethod)
    val inputStream = connection.getInputStream
    val content: String = io.Source.fromInputStream(inputStream).mkString
    if (inputStream != null) inputStream.close()
    content
  }
}
