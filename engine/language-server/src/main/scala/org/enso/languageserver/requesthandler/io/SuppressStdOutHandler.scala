package org.enso.languageserver.requesthandler.io

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.io.InputOutputApi.SuppressStandardOutput
import org.enso.languageserver.io.InputOutputProtocol
import org.enso.languageserver.util.UnhandledLogging

/** A request handler for `io/suppressStandardOutput` commands.
  *
  * @param stdOutController an output redirection controller
  * @param clientId a client requesting redirection
  */
class SuppressStdOutHandler(stdOutController: ActorRef, clientId: ClientId)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive = {
    case Request(SuppressStandardOutput, id, _) =>
      stdOutController ! InputOutputProtocol.SuppressOutput(clientId)
      sender() ! ResponseResult(SuppressStandardOutput, id, Unused)
      context.stop(self)
  }
}

object SuppressStdOutHandler {

  /** Creates a configuration object used to create a [[SuppressStdOutHandler]].
    *
    * @param stdOutController an output redirection controller
    * @param clientId a client requesting redirection
    * @return a configuration object
    */
  def props(stdOutController: ActorRef, clientId: ClientId): Props =
    Props(new SuppressStdOutHandler(stdOutController, clientId))

}
