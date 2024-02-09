package org.enso.downloader.http

import java.net.URI
import java.net.http.HttpRequest
import scala.jdk.CollectionConverters._

/** A simple immutable builder for HTTP requests.
  *
  * It contains very limited functionality that is needed by the APIs used in
  * the launcher. It can be easily extended if necessary.
  */
case class HTTPRequestBuilder private (
  uri: URI,
  headers: Vector[(String, String)],
  internalBuilder: HttpRequest.Builder
) {

  /** Builds a GET request with the specified settings. */
  def GET: HTTPRequest = {
    HTTPRequest(internalBuilder.GET().build())
  }

  /** Adds an additional header that will be included in the request.
    *
    * @param name name of the header
    * @param value the header value
    */
  def addHeader(name: String, value: String): HTTPRequestBuilder =
    copy(headers = headers.appended((name, value)))

  /** Adds multipart form data into `Content-Type: multipart/form-data`
    * @param data
    * @return
    */
  def postMultipartData(data: Map[Object, Object]): HTTPRequestBuilder = {
    val bodyPublisher = MultipartBodyPublisher.ofMimeMultipartData(data.asJava)
    copy(
      internalBuilder = internalBuilder.POST(bodyPublisher)
    )
  }

  def build(): HTTPRequest = {
    headers.foreach { case (name, value) =>
      internalBuilder.header(name, value)
    }
    HTTPRequest(internalBuilder.build())
  }
}

object HTTPRequestBuilder {

  /** Creates a request builder that will send the request for the given URI.
    */
  def fromURI(uri: URI): HTTPRequestBuilder = {
    new HTTPRequestBuilder(uri, Vector.empty, HttpRequest.newBuilder(uri))
  }

  /** Tries to parse the URI provided as a [[String]] and returns a request
    * builder that will send the request to the given `uri`.
    */
  def fromURIString(uri: String): HTTPRequestBuilder =
    fromURI(URIBuilder.fromUri(uri).build())
}
