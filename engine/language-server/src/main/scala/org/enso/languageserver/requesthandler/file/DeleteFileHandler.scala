package org.enso.languageserver.requesthandler.file

import akka.actor._
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileDeletedEvent,
  FileManagerProtocol,
  FileSystemFailureMapper,
  Path
}
import org.enso.languageserver.filemanager.FileManagerApi.DeleteFile
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logger.masking.MaskedString

import scala.concurrent.duration.FiniteDuration

class DeleteFileHandler(requestTimeout: FiniteDuration, fileManager: ActorRef)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(DeleteFile, id, params: DeleteFile.Params) =>
      fileManager ! FileManagerProtocol.DeleteFile(params.path)
      val cancellable = context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable, params.path))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable,
    path: Path
  ): Receive = {
    case Status.Failure(ex) =>
      log.error(
        "Failure during [{}] operation: {}",
        DeleteFile,
        MaskedString(ex.getMessage)
      )
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case FileManagerProtocol.DeleteFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.DeleteFileResult(Right(())) =>
      context.system.eventStream.publish(FileDeletedEvent(path))
      replyTo ! ResponseResult(DeleteFile, id, Unused)
      cancellable.cancel()
      context.stop(self)
  }
}

object DeleteFileHandler {

  def props(timeout: FiniteDuration, fileManager: ActorRef): Props =
    Props(new DeleteFileHandler(timeout, fileManager))

}
