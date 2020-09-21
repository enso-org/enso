package org.enso.launcher.http

import akka.http.scaladsl.model.HttpRequest
import org.apache.http.client.methods.HttpUriRequest

case class HTTPRequest(oldImpl: HttpUriRequest)
