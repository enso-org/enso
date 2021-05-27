package org.enso.languageserver.requesthandler.io

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.io.InputOutputApi.SuppressStandardError
import org.enso.languageserver.io.InputOutputProtocol
import org.enso.languageserver.util.UnhandledLogging

/** A request handler for `io/suppressStandardError` commands.
  *
  * @param stdErrController an output redirection controller
  * @param clientId a client requesting redirection
  */
class SuppressStdErrHandler(stdErrController: ActorRef, clientId: ClientId)
    extends Actor
    with LazyLogging
    with UnhandledLogging {

  override def receive: Receive = {
    case Request(SuppressStandardError, id, _) =>
      stdErrController ! InputOutputProtocol.SuppressOutput(clientId)
      sender() ! ResponseResult(SuppressStandardError, id, Unused)
      context.stop(self)
  }

}

object SuppressStdErrHandler {

  /** Creates a configuration object used to create a [[SuppressStdErrHandler]].
    *
    * @param stdErrController an output redirection controller
    * @param clientId a client requesting redirection
    * @return a configuration object
    */
  def props(stdErrController: ActorRef, clientId: ClientId): Props =
    Props(new SuppressStdErrHandler(stdErrController, clientId))

}
