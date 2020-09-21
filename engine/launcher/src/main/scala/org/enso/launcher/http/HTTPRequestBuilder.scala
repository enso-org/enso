package org.enso.launcher.http

import java.net.URI

import akka.http.scaladsl.model.{HttpMethod, HttpMethods, HttpRequest}
import org.apache.http.client.methods.{HttpUriRequest, RequestBuilder}

/**
  * A simple immutable builder for HTTP requests.
  *
  * It contains very limited functionality that is needed by the APIs used in
  * the launcher. It can be easily extended if necessary.
  */
case class HTTPRequestBuilder private (
  uri: URI,
  headers: Vector[(String, String)]
) {

  /**
    * Builds a GET request with the specified settings.
    */
  def GET: HTTPRequest = build(RequestBuilder.get(), HttpMethods.GET)

  /**
    * Adds an additional header that will be included in the request.
    *
    * @param name name of the header
    * @param value the header value
    */
  def addHeader(name: String, value: String): HTTPRequestBuilder =
    copy(headers = headers.appended((name, value)))

  private def build(
    oldStuff: RequestBuilder,
    method: HttpMethod
  ): HTTPRequest = {
    val withUri = oldStuff.setUri(uri)
    val withHeaders = headers.foldLeft(withUri)((builder, header) =>
      builder.addHeader(header._1, header._2)
    )
    val oldRequest = withHeaders.build() // TODO remove

    HTTPRequest(oldRequest)
  }
}

object HTTPRequestBuilder {

  /**
    * Creates a request builder that will send the request for the given URI.
    */
  def fromURI(uri: URI): HTTPRequestBuilder =
    new HTTPRequestBuilder(uri, Vector.empty)

  /**
    * Tries to parse the URI provided as a [[String]] and returns a request
    * builder that will send the request to the given `uri`.
    */
  def fromURIString(uri: String): HTTPRequestBuilder =
    fromURI(new URI(uri))
}
