package org.enso.languageserver.requesthandler.text

import akka.actor.{Actor, ActorRef, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.text.TextApi.{CloseFile, FileNotOpenedError}
import org.enso.languageserver.text.{TextApi, TextProtocol}
import org.enso.languageserver.text.TextProtocol.{FileClosed, FileNotOpened}
import org.enso.languageserver.util.{
  RequestHandlerWithRetries,
  UnhandledLogging
}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration

/** A request handler for `text/closeFile` commands.
  *
  * @param bufferRegistry a router that dispatches text editing requests
  * @param timeout a request timeout
  * @param rpcSession an object representing a client connected to the language server
  */
class CloseFileHandler(
  bufferRegistry: ActorRef,
  timeout: FiniteDuration,
  rpcSession: JsonSession
) extends RequestHandlerWithRetries[
      Request[CloseFile.type, CloseFile.Params],
      FileClosed.type,
      FileNotOpened.type,
      TextProtocol.CloseFile
    ](bufferRegistry, timeout, 5)
    with Actor
    with LazyLogging
    with UnhandledLogging {

  override protected def request(
    msg: Request[TextApi.CloseFile.type, CloseFile.Params]
  ): TextProtocol.CloseFile = {
    TextProtocol.CloseFile(rpcSession.clientId, msg.params.path)
  }

  override protected def positiveResponse(
    replyTo: ActorRef,
    initialMsg: Request[TextApi.CloseFile.type, CloseFile.Params],
    msg: TextProtocol.FileClosed.type
  ): Unit = {
    replyTo ! ResponseResult(CloseFile, initialMsg.id, Unused)
  }

  override protected def negativeResponse(
    replyTo: ActorRef,
    initialMsg: Request[TextApi.CloseFile.type, CloseFile.Params],
    error: TextProtocol.FileNotOpened.type
  )(implicit ec: ExecutionContext): Unit = {
    replyTo ! ResponseError(Some(initialMsg.id), FileNotOpenedError)
  }
}

object CloseFileHandler {

  /** Creates a configuration object used to create a [[CloseFileHandler]]
    *
    * @param bufferRegistry a router that dispatches text editing requests
    * @param requestTimeout a request timeout
    * @param rpcSession an object representing a client connected to the language server
    * @return a configuration object
    */
  def props(
    bufferRegistry: ActorRef,
    requestTimeout: FiniteDuration,
    rpcSession: JsonSession
  ): Props =
    Props(new CloseFileHandler(bufferRegistry, requestTimeout, rpcSession))

}
