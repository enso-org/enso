package org.enso.languageserver.requesthandler.file

import akka.actor.{Actor, ActorRef, Cancellable, Props, Status}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.Errors.RequestTimeout
import org.enso.jsonrpc._
import org.enso.languageserver.filemanager.FileManagerApi.ChecksumFile
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.logger.masking.MaskedString

import scala.concurrent.duration.FiniteDuration

/** A request handler for the `file/checksum` command.
  *
  * @param requestTimeout a request timeout
  * @param fileManager a file system manager actor
  */
class ChecksumFileHandler(
  requestTimeout: FiniteDuration,
  fileManager: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(ChecksumFile, id, params: ChecksumFile.Params) =>
      fileManager ! FileManagerProtocol.ChecksumFileRequest(params.path)
      val cancellable = context.system.scheduler.scheduleOnce(
        requestTimeout,
        self,
        RequestTimeout
      )
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
        ChecksumFile,
        MaskedString(ex.getMessage)
      )
      replyTo ! ResponseError(Some(id), Errors.ServiceError)
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.error("Request [{}] timed out.", id)
      replyTo ! ResponseError(Some(id), Errors.RequestTimeout)
      context.stop(self)

    case FileManagerProtocol.ChecksumFileResponse(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.ChecksumFileResponse(Right(result)) =>
      replyTo ! ResponseResult(ChecksumFile, id, ChecksumFile.Result(result))
      cancellable.cancel()
      context.stop(self)
  }
}
object ChecksumFileHandler {

  /** Creates a configuration object used to create a [[ChecksumFileHandler]].
    *
    * @param timeout a request timeout
    * @param fileManager the file manager actor
    * @return an actor for handling checksum file commands
    */
  def props(timeout: FiniteDuration, fileManager: ActorRef): Props = Props(
    new ChecksumFileHandler(timeout, fileManager)
  )
}
