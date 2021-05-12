package org.enso.languageserver.requesthandler.file

import akka.actor._
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.filemanager.FileManagerApi.ExistsFile
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logger.masking.MaskedString

import scala.concurrent.duration.FiniteDuration

class ExistsFileHandler(requestTimeout: FiniteDuration, fileManager: ActorRef)
    extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ExistsFile, id, params: ExistsFile.Params) =>
      fileManager ! FileManagerProtocol.ExistsFile(params.path)
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
      log.error(
        "Failure during [{}] operation: {}",
        ExistsFile,
        MaskedString(ex.getMessage)
      )
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case FileManagerProtocol.ExistsFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.ExistsFileResult(Right(result)) =>
      replyTo ! ResponseResult(ExistsFile, id, ExistsFile.Result(result))
      cancellable.cancel()
      context.stop(self)
  }
}

object ExistsFileHandler {

  def props(timeout: FiniteDuration, fileManager: ActorRef): Props =
    Props(new ExistsFileHandler(timeout, fileManager))

}
