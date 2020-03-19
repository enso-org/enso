package org.enso.languageserver.requesthandler.file

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Status}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.filemanager.FileManagerApi.ExistsFile
import org.enso.languageserver.requesthandler.RequestTimeout

import scala.concurrent.duration.FiniteDuration

class ExistsFileHandler(requestTimeout: FiniteDuration, fileManager: ActorRef)
    extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ExistsFile, id, params: ExistsFile.Params) =>
      fileManager ! FileManagerProtocol.ExistsFile(params.path)
      context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case Status.Failure(ex) =>
      log.error(s"Failure during $ExistsFile operation:", ex)
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case FileManagerProtocol.ExistsFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      context.stop(self)

    case FileManagerProtocol.ExistsFileResult(Right(result)) =>
      replyTo ! ResponseResult(ExistsFile, id, ExistsFile.Result(result))
      context.stop(self)
  }
}

object ExistsFileHandler {

  def props(timeout: FiniteDuration, fileManager: ActorRef): Props =
    Props(new ExistsFileHandler(timeout, fileManager))

}
