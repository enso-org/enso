package org.enso.jsonrpc

import akka.http.scaladsl.server.Route

/** An interface for all HTTP services.
  */
trait Endpoint {

  /** A definition of HTTP service.
    *
    * @return
    */
  def route: Route

}
