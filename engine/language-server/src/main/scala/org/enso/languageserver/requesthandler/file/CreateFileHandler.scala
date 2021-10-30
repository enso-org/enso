package org.enso.languageserver.requesthandler.file

import akka.actor._
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.filemanager.FileManagerApi.CreateFile
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logger.masking.MaskedString

import scala.concurrent.duration.FiniteDuration

class CreateFileHandler(requestTimeout: FiniteDuration, fileManager: ActorRef)
    extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(CreateFile, id, params: CreateFile.Params) =>
      fileManager ! FileManagerProtocol.CreateFile(params.`object`)
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
        "Failure during [{}] operation: {}",
        CreateFile,
        MaskedString(ex.getMessage)
      )
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case FileManagerProtocol.CreateFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.CreateFileResult(Right(())) =>
      replyTo ! ResponseResult(CreateFile, id, Unused)
      cancellable.cancel()
      context.stop(self)
  }
}

object CreateFileHandler {

  def props(timeout: FiniteDuration, fileManager: ActorRef): Props =
    Props(new CreateFileHandler(timeout, fileManager))

}
