package code.Rest

import net.liftweb.util.Props

/**
  * Created by philippederome on 2015-12-11.
  */
trait pagerRestClient extends RestClient {
  // Following values must be read as config externally. We don't mean to rely on defaults below, rather properties should be set sensibly.
  def MaxPerPage = Props.getInt("product.lcboMaxPerPage", 0)

  def MinPerPage = Props.getInt("product.lcboMinPerPage", 0)

  val LcboDomainURL = Props.get("lcboDomainURL", "http://") // set it!

  // need good defaults here however.
  val HttpClientConnTimeOut = Props.getInt("http.ClientConnTimeOut", 5000)
  val HttpClientReadTimeOut = Props.getInt("http.ClientReadTimeOut", HttpClientConnTimeOut)

  /**
    * streams as String a parameter tag value pair prefixed with & as building block for a URL query String.
    * Assumes not first one (uses &). Improve! First approximation towards a better solution.
    * @param name name of optional parameter
    * @param value value of optional parameter
    * @return     &<name>=<value>         Assumes value is meaningful with printable data, but does not validate it.
    */
  def additionalParam[A](name: String, value: A): String = s"&$name=$value"

}
