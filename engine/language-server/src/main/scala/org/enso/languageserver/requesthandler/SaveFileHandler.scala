package org.enso.languageserver.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.data.Client
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.text.TextApi.{
  FileNotOpenedError,
  InvalidVersionError,
  SaveFile,
  WriteDeniedError
}
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.text.TextProtocol._

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `text/save` commands.
  *
  * @param bufferRegistry a router that dispatches text editing requests
  * @param timeout a request timeout
  * @param client an object representing a client connected to the language server
  */
class SaveFileHandler(
  bufferRegistry: ActorRef,
  timeout: FiniteDuration,
  client: Client
) extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(SaveFile, id, params: SaveFile.Params) =>
      bufferRegistry ! TextProtocol.SaveFile(
        client.id,
        params.path,
        params.currentVersion
      )
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case RequestTimeout =>
      log.error(s"Saving file for ${client.id} timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case FileSaved =>
      replyTo ! ResponseResult(SaveFile, id, Unused)
      context.stop(self)

    case SaveFailed(fsFailure) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(fsFailure)
      )
      context.stop(self)

    case SaveDenied =>
      replyTo ! ResponseError(Some(id), WriteDeniedError)
      context.stop(self)

    case SaveFileInvalidVersion(clientVersion, serverVersion) =>
      replyTo ! ResponseError(
        Some(id),
        InvalidVersionError(clientVersion, serverVersion)
      )
      context.stop(self)

    case FileNotOpened =>
      replyTo ! ResponseError(Some(id), FileNotOpenedError)
      context.stop(self)
  }

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

}

object SaveFileHandler {

  /**
    * Creates a configuration object used to create a [[SaveFileHandler]].
    *
    * @param bufferRegistry a router that dispatches text editing requests
    * @param requestTimeout a request timeout
    * @param client an object representing a client connected to the language server
    * @return a configuration object
    */
  def props(
    bufferRegistry: ActorRef,
    requestTimeout: FiniteDuration,
    client: Client
  ): Props = Props(new SaveFileHandler(bufferRegistry, requestTimeout, client))

}
