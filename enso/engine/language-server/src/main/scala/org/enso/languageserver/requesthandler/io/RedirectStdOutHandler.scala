package org.enso.languageserver.requesthandler.io

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.{Request, ResponseResult, Unused}
import org.enso.languageserver.data.ClientId
import org.enso.languageserver.io.InputOutputApi.RedirectStandardOutput
import org.enso.languageserver.io.InputOutputProtocol
import org.enso.languageserver.util.UnhandledLogging

/** A request handler for `io/redirectStandardOutput` commands.
  *
  * @param stdOutController an output redirection controller
  * @param clientId a client requesting redirection
  */
class RedirectStdOutHandler(stdOutController: ActorRef, clientId: ClientId)
    extends Actor
    with LazyLogging
    with UnhandledLogging {

  override def receive: Receive = {
    case Request(RedirectStandardOutput, id, _) =>
      stdOutController ! InputOutputProtocol.RedirectOutput(clientId)
      sender() ! ResponseResult(RedirectStandardOutput, id, Unused)
      context.stop(self)
  }

}

object RedirectStdOutHandler {

  /** Creates a configuration object used to create a [[RedirectStdOutHandler]].
    *
    * @param stdOutController an output redirection controller
    * @param clientId a client requesting redirection
    * @return a configuration object
    */
  def props(stdOutController: ActorRef, clientId: ClientId): Props =
    Props(new RedirectStdOutHandler(stdOutController, clientId))

}
