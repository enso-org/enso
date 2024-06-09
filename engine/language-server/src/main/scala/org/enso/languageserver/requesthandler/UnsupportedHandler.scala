package org.enso.languageserver.requesthandler

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.{Errors, Method, Request, ResponseError}
import org.enso.languageserver.util.UnhandledLogging

final class UnsupportedHandler(method: Method)
    extends Actor
    with LazyLogging
    with UnhandledLogging {

  override def receive: Receive = { case Request(`method`, id, _) =>
    sender() ! ResponseError(
      Some(id),
      Errors.MethodNotFound
    )
  }
}
