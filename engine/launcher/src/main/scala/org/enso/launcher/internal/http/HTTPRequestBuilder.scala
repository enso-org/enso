package org.enso.launcher.internal.http

import java.net.URI

import org.apache.http.client.methods.{HttpUriRequest, RequestBuilder}

case class HTTPRequestBuilder private (
  uri: URI,
  headers: Vector[(String, String)]
) {
  def GET: HttpUriRequest = build(RequestBuilder.get())

  def setHeader(key: String, value: String): HTTPRequestBuilder =
    copy(headers = headers.appended((key, value)))

  private def build(requestBuilder: RequestBuilder): HttpUriRequest = {

    val withUri = requestBuilder.setUri(uri)
    val withHeaders = headers.foldLeft(withUri)((builder, header) =>
      builder.addHeader(header._1, header._2)
    )
    withHeaders.build()
  }
}

object HTTPRequestBuilder {
  def fromURI(uri: URI): HTTPRequestBuilder =
    new HTTPRequestBuilder(uri, Vector.empty)

  def fromURL(url: String): HTTPRequestBuilder =
    new HTTPRequestBuilder(new URI(url), Vector.empty)
}
