package org.enso.languageserver.requesthandler.io

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.io.InputOutputApi.RedirectStandardError
import org.enso.languageserver.io.InputOutputProtocol
import org.enso.languageserver.util.UnhandledLogging

/** A request handler for `io/redirectStandardError` commands.
  *
  * @param stdErrController an output redirection controller
  * @param clientId a client requesting redirection
  */
class RedirectStdErrHandler(stdErrController: ActorRef, clientId: ClientId)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = {
    case Request(RedirectStandardError, id, _) =>
      stdErrController ! InputOutputProtocol.RedirectOutput(clientId)
      sender() ! ResponseResult(RedirectStandardError, id, Unused)
      context.stop(self)
  }

}

object RedirectStdErrHandler {

  /** Creates a configuration object used to create a [[RedirectStdErrHandler]].
    *
    * @param stdErrController an output redirection controller
    * @param clientId a client requesting redirection
    * @return a configuration object
    */
  def props(stdErrController: ActorRef, clientId: ClientId): Props =
    Props(new RedirectStdErrHandler(stdErrController, clientId))

}
