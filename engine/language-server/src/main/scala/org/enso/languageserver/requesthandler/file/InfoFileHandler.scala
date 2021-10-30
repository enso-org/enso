package org.enso.languageserver.requesthandler.file

import akka.actor._
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.filemanager.FileManagerApi.InfoFile
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logger.masking.MaskedString

import scala.concurrent.duration.FiniteDuration

/** A request handler for `file/info` command.
  *
  * @param requestTimeout a request timeout
  * @param fileManager a file system manager actor
  */
class InfoFileHandler(requestTimeout: FiniteDuration, fileManager: ActorRef)
    extends Actor
    with LazyLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(InfoFile, id, params: InfoFile.Params) =>
      fileManager ! FileManagerProtocol.InfoFile(params.path)
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
        InfoFile,
        MaskedString(ex.getMessage)
      )
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case FileManagerProtocol.InfoFileResult(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.InfoFileResult(Right(result)) =>
      replyTo ! ResponseResult(InfoFile, id, InfoFile.Result(result))
      cancellable.cancel()
      context.stop(self)
  }
}

object InfoFileHandler {

  /** Creates a configuration object used to create a [[InfoFileHandler]].
    *
    * @param timeout a request timeout
    * @param fileManager a file system manager actor
    */
  def props(timeout: FiniteDuration, fileManager: ActorRef): Props =
    Props(new InfoFileHandler(timeout, fileManager))

}
