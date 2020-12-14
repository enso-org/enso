package org.enso.jsonrpc

import akka.http.scaladsl.server.Route

trait Endpoint {

  def route: Route

}
