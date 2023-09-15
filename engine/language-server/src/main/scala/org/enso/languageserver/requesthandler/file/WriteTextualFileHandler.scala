package org.enso.languageserver.requesthandler.file

import akka.actor._
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.filemanager.FileManagerApi.WriteFile
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logger.masking.MaskedString

import scala.concurrent.duration.FiniteDuration

class WriteTextualFileHandler(
  requestTimeout: FiniteDuration,
  fileManager: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(WriteFile, id, params: WriteFile.Params) =>
      fileManager ! FileManagerProtocol.WriteFile(params.path, params.contents)
      val cancellable = context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender(), cancellable))
  }

  private def responseStage(
    id: Id,
    replyTo: ActorRef,
    cancellable: Cancellable
  ): Receive = {
    case Status.Failure(ex) =>
      logger.error(
        s"Failure during [{}] operation: {}",
        WriteFile,
        MaskedString(ex.getMessage)
      )
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      context.stop(self)

    case FileManagerProtocol.WriteFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.WriteFileResult(Right(_)) =>
      replyTo ! ResponseResult(WriteFile, id, Unused)
      cancellable.cancel()
      context.stop(self)
  }
}

object WriteTextualFileHandler {

  def props(timeout: FiniteDuration, fileManager: ActorRef): Props =
    Props(new WriteTextualFileHandler(timeout, fileManager))

}
