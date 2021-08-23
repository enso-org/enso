package org.enso.lockmanager.client

import org.enso.polyglot.runtime.Runtime.{ApiRequest, ApiResponse}

import scala.concurrent.Future

trait RuntimeServerConnectionEndpoint {
  def sendRequest(msg: ApiRequest): Future[ApiResponse]
}
