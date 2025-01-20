package org.enso.languageserver.requesthandler.file

import akka.actor._
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.filemanager.FileManagerApi.ReadFile
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.text.TextProtocol.{
  ReadCollaborativeBuffer,
  ReadCollaborativeBufferResult
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logger.masking.MaskedString

import scala.concurrent.duration.FiniteDuration

class ReadTextualFileHandler(
  requestTimeout: FiniteDuration,
  bufferRegistry: ActorRef,
  fileManager: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ReadFile, id, params: ReadFile.Params) =>
      bufferRegistry ! ReadCollaborativeBuffer(params.path)
      val cancellable = context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(requestBufferStage(id, sender(), cancellable, params))
  }

  private def requestBufferStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable,
    params: ReadFile.Params
  ): Receive = {
    case Status.Failure(ex) =>
      logger.error(
        "Failure during [{}] operation: {}",
        ReadFile,
        MaskedString(ex.getMessage)
      )
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.warn(
        "Read collaborative buffer timed out. " +
        "Falling back to reading file contents."
      )
      fileManager ! FileManagerProtocol.ReadFile(params.path)
      val cancellable = context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, replyTo, cancellable))

    case ReadCollaborativeBufferResult(None) =>
      cancellable.cancel()
      fileManager ! FileManagerProtocol.ReadFile(params.path)
      val newCancellable = context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, replyTo, newCancellable))

    case ReadCollaborativeBufferResult(Some(buffer)) =>
      replyTo ! ResponseResult(
        ReadFile,
        id,
        ReadFile.Result(buffer.contents.toString)
      )
      cancellable.cancel()
      context.stop(self)
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case Status.Failure(ex) =>
      logger.error(
        "Failure during [{}] operation: {}",
        ReadFile,
        MaskedString(ex.getMessage)
      )
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case FileManagerProtocol.ReadTextualFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.ReadTextualFileResult(Right(file)) =>
      replyTo ! ResponseResult(ReadFile, id, ReadFile.Result(file.content))
      cancellable.cancel()
      context.stop(self)
  }
}

object ReadTextualFileHandler {

  def props(
    timeout: FiniteDuration,
    bufferRegistry: ActorRef,
    fileManager: ActorRef
  ): Props =
    Props(new ReadTextualFileHandler(timeout, bufferRegistry, fileManager))

}
