package org.enso.languageserver.requesthandler.file

import akka.actor.{Actor, ActorRef, Cancellable, Props, Status}
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.Errors.RequestTimeout
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper,
  ReadOutOfBounds
}
import org.enso.languageserver.protocol.binary.factory.{
  ChecksumBytesReplyFactory,
  ErrorFactory
}
import org.enso.languageserver.protocol.binary.{
  ChecksumBytesCommand,
  EnsoUUID,
  FileSegment,
  InboundMessage
}
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.util.file.PathUtils
import org.enso.logger.masking.MaskedString

import scala.concurrent.duration.FiniteDuration

/** A handler for a checksum bytes request.
  *
  * @param requestTimeout a request timeout
  * @param fileManager a reference to the file-manager actor
  * @param replyTo the actor to reply to
  */
class ChecksumBytesHandler(
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
      msg.payload(new ChecksumBytesCommand).asInstanceOf[ChecksumBytesCommand]
    val segment = payload.segment
    fileManager ! FileManagerProtocol.ChecksumBytesRequest(
      ChecksumBytesHandler.convertFileSegment(segment)
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
        "Failure during the ChecksumBytes operation: {}",
        MaskedString(ex.getMessage)
      )

      val response = ErrorFactory.createServiceError(Some(requestId))
      replyTo ! response
      cancellable.cancel()
      context.stop(self)

    case RequestTimeout =>
      logger.error("Request ChecksumBytes [{}] timed out.", requestId)
      val response = ErrorFactory.createServiceError(Some(requestId))
      replyTo ! response
      context.stop(self)

    case FileManagerProtocol.ChecksumBytesResponse(Left(failure))
        if failure.hasData =>
      failure match {
        case ReadOutOfBounds(fileLength) =>
          val response =
            ErrorFactory.createReadOutOfBoundsError(fileLength, Some(requestId))
          replyTo ! response
          cancellable.cancel()
          context.stop(self)
        case _ =>
          logger.error("The impossible happened in request [{}].", requestId)
          val response = ErrorFactory.createServiceError(Some(requestId))
          replyTo ! response
          context.stop(self)
      }

    case FileManagerProtocol.ChecksumBytesResponse(Left(failure))
        if !failure.hasData =>
      val error = FileSystemFailureMapper.mapFailure(failure)
      val response = ErrorFactory.createGenericError(
        error.code,
        error.message,
        maybeCorrelationId = Some(requestId)
      )
      replyTo ! response
      cancellable.cancel()
      context.stop(self)

    case FileManagerProtocol.ChecksumBytesResponse(Right(checksum)) =>
      val response = ChecksumBytesReplyFactory.create(checksum, requestId)
      replyTo ! response
      cancellable.cancel()
      context.stop(self)
  }
}
object ChecksumBytesHandler {

  /** Creates a configuration object used to create a [[ChecksumBytesHandler]].
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
    Props(new ChecksumBytesHandler(timeout, fileManager, replyTo))
  }

  /** Converts from a binary file segment to a protocol one.
    *
    * @param segment the segment to convert
    * @return `segment` using protocol types
    */
  def convertFileSegment(
    segment: FileSegment
  ): FileManagerProtocol.Data.FileSegment = {
    FileManagerProtocol.Data.FileSegment(
      PathUtils.convertBinaryPath(segment.path),
      segment.byteOffset(),
      segment.length()
    )
  }
}
