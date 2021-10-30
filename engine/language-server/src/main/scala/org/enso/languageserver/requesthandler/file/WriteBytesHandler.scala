package org.enso.languageserver.requesthandler.file

import akka.actor.{Actor, ActorRef, Cancellable, Props, Status}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.Errors.RequestTimeout
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.protocol.binary.factory.{
  ErrorFactory,
  WriteBytesReplyFactory
}
import org.enso.languageserver.protocol.binary.{
  EnsoUUID,
  InboundMessage,
  WriteBytesCommand
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.util.file.PathUtils
import org.enso.logger.masking.MaskedString

import scala.concurrent.duration.FiniteDuration

/** A handler for a write bytes request
  *
  * @param requestTimeout a request timeout
  * @param fileManager a reference to the file-manager actor
  * @param replyTo the actor to reply to
  */
class WriteBytesHandler(
  requestTimeout: FiniteDuration,
  fileManager: ActorRef,
  replyTo: ActorRef
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = { case msg: InboundMessage =>
    val payload =
      msg.payload(new WriteBytesCommand).asInstanceOf[WriteBytesCommand]
    val byteBuf = payload.bytesAsByteBuffer()
    val bytes   = new Array[Byte](byteBuf.remaining())
    byteBuf.get(bytes)
    fileManager ! FileManagerProtocol.WriteBytesRequest(
      PathUtils.convertBinaryPath(payload.path()),
      payload.byteOffset(),
      payload.overwriteExisting(),
      bytes
    )
    val cancellable = context.system.scheduler.scheduleOnce(
      requestTimeout,
      self,
      RequestTimeout
    )
    context.become(responseStage(msg.messageId(), cancellable))
  }

  private def responseStage(
    requestId: EnsoUUID,
    cancellable: Cancellable
  ): Receive = {
    case Status.Failure(ex) =>
      logger.error(
        "Failure during the WriteBytes operation: {}",
        MaskedString(ex.getMessage)
      )

      val response = ErrorFactory.createServiceError(Some(requestId))
      replyTo ! response
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.error("Request WriteBytes [{}] timed out.", requestId)
      val response = ErrorFactory.createServiceError(Some(requestId))
      replyTo ! response
      context.stop(self)

    case FileManagerProtocol.WriteBytesResponse(Left(failure)) =>
      val error = FileSystemFailureMapper.mapFailure(failure)
      val response = ErrorFactory.createGenericError(
        error.code,
        error.message,
        maybeCorrelationId = Some(requestId)
      )
      replyTo ! response
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.WriteBytesResponse(Right(checksum)) =>
      val response = WriteBytesReplyFactory.create(checksum, requestId)
      replyTo ! response
      cancellable.cancel()
      context.stop(self)
  }
}
object WriteBytesHandler {

  /** Creates a configuration object used to create a [[WriteBytesHandler]].
    *
    * @param timeout the request timeout
    * @param fileManager the file system manager actor
    * @param replyTo the outbound channel delivering replies to the client
    * @return a configuration object
    */
  def props(
    timeout: FiniteDuration,
    fileManager: ActorRef,
    replyTo: ActorRef
  ): Props = {
    Props(new WriteBytesHandler(timeout, fileManager, replyTo))
  }
}
