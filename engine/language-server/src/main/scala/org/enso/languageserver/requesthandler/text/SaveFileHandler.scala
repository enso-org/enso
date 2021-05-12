package org.enso.languageserver.requesthandler.text

import akka.actor.{Actor, ActorLogging, ActorRef, Cancellable, Props}
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.session.JsonSession
import org.enso.languageserver.text.TextApi.{
  FileNotOpenedError,
  InvalidVersionError,
  SaveFile,
  WriteDeniedError
}
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.text.TextProtocol._
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

/** A request handler for `text/save` commands.
  *
  * @param bufferRegistry a router that dispatches text editing requests
  * @param timeout a request timeout
  * @param rpcSession an object representing a client connected to the language server
  */
class SaveFileHandler(
  bufferRegistry: ActorRef,
  timeout: FiniteDuration,
  rpcSession: JsonSession
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(SaveFile, id, params: SaveFile.Params) =>
      bufferRegistry ! TextProtocol.SaveFile(
        rpcSession.clientId,
        params.path,
        params.currentVersion
      )
      val cancellable =
        context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case RequestTimeout =>
      log.error(
        "Saving file request [{}] for [{}] timed out.",
        id,
        rpcSession.clientId
      )
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case FileSaved =>
      replyTo ! ResponseResult(SaveFile, id, Unused)
      cancellable.cancel()
      context.stop(self)

    case SaveFailed(fsFailure) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(fsFailure)
      )
      cancellable.cancel()
      context.stop(self)

    case SaveDenied =>
      replyTo ! ResponseError(Some(id), WriteDeniedError)
      cancellable.cancel()
      context.stop(self)

    case SaveFileInvalidVersion(clientVersion, serverVersion) =>
      replyTo ! ResponseError(
        Some(id),
        InvalidVersionError(clientVersion, serverVersion)
      )
      cancellable.cancel()
      context.stop(self)

    case FileNotOpened =>
      replyTo ! ResponseError(Some(id), FileNotOpenedError)
      cancellable.cancel()
      context.stop(self)
  }
}

object SaveFileHandler {

  /** Creates a configuration object used to create a [[SaveFileHandler]].
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
    Props(new SaveFileHandler(bufferRegistry, requestTimeout, rpcSession))

}
