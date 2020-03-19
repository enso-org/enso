package org.enso.languageserver.requesthandler.file

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Status}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.filemanager.FileManagerApi.CreateFile
import org.enso.languageserver.requesthandler.RequestTimeout

import scala.concurrent.duration.FiniteDuration

class CreateFileHandler(requestTimeout: FiniteDuration, fileManager: ActorRef)
    extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(CreateFile, id, params: CreateFile.Params) =>
      fileManager ! FileManagerProtocol.CreateFile(params.`object`)
      context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case Status.Failure(ex) =>
      log.error(s"Failure during $CreateFile operation:", ex)
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case FileManagerProtocol.CreateFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      context.stop(self)

    case FileManagerProtocol.CreateFileResult(Right(())) =>
      replyTo ! ResponseResult(CreateFile, id, Unused)
      context.stop(self)
  }
}

object CreateFileHandler {

  def props(timeout: FiniteDuration, fileManager: ActorRef): Props =
    Props(new CreateFileHandler(timeout, fileManager))

}
