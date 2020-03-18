package org.enso.languageserver.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc.{Id, Request, ResponseError, ResponseResult}
import org.enso.languageserver.data.Client
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.text.TextApi.OpenFile
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.text.TextProtocol.{
  OpenFileResponse,
  OpenFileResult
}

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for `text/openFile` commands.
  *
  * @param bufferRegistry a router that dispatches text editing requests
  * @param timeout a request timeout
  * @param client an object representing a client connected to the language server
  */
class OpenFileHandler(
  bufferRegistry: ActorRef,
  timeout: FiniteDuration,
  client: Client
) extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(OpenFile, id, params: OpenFile.Params) =>
      bufferRegistry ! TextProtocol.OpenFile(client, params.path)
      context.system.scheduler.scheduleOnce(timeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case RequestTimeout =>
      log.error(s"Opening file for ${client.id} timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case OpenFileResponse(Right(OpenFileResult(buffer, capability))) =>
      replyTo ! ResponseResult(
        OpenFile,
        id,
        OpenFile
          .Result(capability, buffer.contents.toString, buffer.version)
      )
      context.stop(self)

    case OpenFileResponse(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      context.stop(self)
  }

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

}

object OpenFileHandler {

  /**
    * Creates a configuration object used to create a [[OpenFileHandler]]
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
  ): Props = Props(new OpenFileHandler(bufferRegistry, requestTimeout, client))

}
