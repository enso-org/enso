package org.enso.languageserver.requesthandler.file

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Status}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.filemanager.FileManagerApi.ReadFile
import org.enso.languageserver.requesthandler.RequestTimeout

import scala.concurrent.duration.FiniteDuration

class ReadFileHandler(requestTimeout: FiniteDuration, fileManager: ActorRef)
    extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ReadFile, id, params: ReadFile.Params) =>
      fileManager ! FileManagerProtocol.ReadFile(params.path)
      context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case Status.Failure(ex) =>
      log.error(s"Failure during $ReadFile operation:", ex)
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case FileManagerProtocol.ReadFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      context.stop(self)

    case FileManagerProtocol.ReadFileResult(Right(content)) =>
      replyTo ! ResponseResult(ReadFile, id, ReadFile.Result(content))
      context.stop(self)
  }
}

object ReadFileHandler {

  def props(timeout: FiniteDuration, fileManager: ActorRef): Props =
    Props(new ReadFileHandler(timeout, fileManager))

}
