package org.enso.languageserver.requesthandler.file

import akka.actor._
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.protocol.data.envelope.InboundMessage
import org.enso.languageserver.protocol.data.factory.{
  ErrorFactory,
  FileContentsReplyFactory
}
import org.enso.languageserver.protocol.data.filemanager.ReadFileCommand
import org.enso.languageserver.protocol.data.util.EnsoUUID
import org.enso.languageserver.requesthandler.RequestTimeout
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.util.file.PathUtils

import scala.concurrent.duration.FiniteDuration

/**
  * A request handler for [[ReadFileCommand]].
  *
  * @param requestTimeout a request timeout
  * @param fileManager a file system manager actor
  * @param replyTo the outbound channel delivering replies to the client
  */
class ReadBinaryFileHandler(
  requestTimeout: FiniteDuration,
  fileManager: ActorRef,
  replyTo: ActorRef
) extends Actor
    with ActorLogging
    with UnhandledLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case msg: InboundMessage =>
      val payload =
        msg.payload(new ReadFileCommand).asInstanceOf[ReadFileCommand]
      val path = PathUtils.convertBinaryPath(payload.path())
      fileManager ! FileManagerProtocol.ReadBinaryFile(path)
      val cancellable = context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(msg.requestId(), cancellable))
  }

  private def responseStage(
    requestId: EnsoUUID,
    cancellable: Cancellable
  ): Receive = {
    case Status.Failure(ex) =>
      log.error(s"Failure during ReadBinaryFile operation:", ex)
      val packet = ErrorFactory.createServiceError(Some(requestId))
      replyTo ! packet
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      log.error(s"Request ReadBinaryFile timed out")
      val packet = ErrorFactory.createServiceError(Some(requestId))
      replyTo ! packet
      context.stop(self)

    case FileManagerProtocol.ReadBinaryFileResult(Left(failure)) =>
      val error = FileSystemFailureMapper.mapFailure(failure)
      val packet = ErrorFactory.createGenericError(
        error.code,
        error.message,
        Some(requestId)
      )
      replyTo ! packet
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.ReadBinaryFileResult(Right(fileContent)) =>
      val packet =
        FileContentsReplyFactory.createPacket(fileContent.contents, requestId)
      replyTo ! packet
      cancellable.cancel()
      context.stop(self)
  }

}

object ReadBinaryFileHandler {

  /**
    * Creates a configuration object used to create a [[ReadBinaryFileHandler]]
    *
    * @param timeout a request timeout
    * @param fileManager a file system manager actor
    * @param replyTo the outbound channel delivering replies to the client
    */
  def props(
    timeout: FiniteDuration,
    fileManager: ActorRef,
    replyTo: ActorRef
  ): Props =
    Props(new ReadBinaryFileHandler(timeout, fileManager, replyTo: ActorRef))

}
