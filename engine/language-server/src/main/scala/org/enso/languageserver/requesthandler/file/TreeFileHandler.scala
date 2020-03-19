package org.enso.languageserver.requesthandler.file

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Status}
import org.enso.jsonrpc.Errors.ServiceError
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.filemanager.FileManagerApi.TreeFile
import org.enso.languageserver.requesthandler.RequestTimeout

import scala.concurrent.duration.FiniteDuration

class TreeFileHandler(requestTimeout: FiniteDuration, fileManager: ActorRef)
    extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(TreeFile, id, params: TreeFile.Params) =>
      fileManager ! FileManagerProtocol.TreeFile(params.path, params.depth)
      context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case Status.Failure(ex) =>
      log.error(s"Failure during $TreeFile operation:", ex)
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request $id timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case FileManagerProtocol.TreeFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      context.stop(self)

    case FileManagerProtocol.TreeFileResult(Right(result)) =>
      replyTo ! ResponseResult(TreeFile, id, TreeFile.Result(result))
      context.stop(self)
  }
}

object TreeFileHandler {

  def props(timeout: FiniteDuration, fileManager: ActorRef): Props =
    Props(new TreeFileHandler(timeout, fileManager))

}
