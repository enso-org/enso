package org.enso.lockmanager.client

import org.enso.polyglot.runtime.Runtime.{ApiRequest, ApiResponse}

import scala.concurrent.Future

/** A connection with the Language Server which provides a simple Future-based
  * interface for sending requests and receiving corresponding responses.
  */
trait RuntimeServerConnectionEndpoint {

  /** Sends a request to the language server and returns a Future that is
    * completed once a correlated response has been received.
    */
  def sendRequest(msg: ApiRequest): Future[ApiResponse]
}
