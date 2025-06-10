package org.enso.downloader.http

import java.net.http.HttpRequest

/** Wraps an underlying HTTP request implementation to make the outside API
  * independent of the internal implementation.
  */
case class HTTPRequest(requestImpl: HttpRequest) {

  /** Returns the method of this request. */
  def method: String = requestImpl.method()

  /** Returns the URI of this request as string, used e.g. for logging. */
  def uri: String = requestImpl.uri().toString
}
