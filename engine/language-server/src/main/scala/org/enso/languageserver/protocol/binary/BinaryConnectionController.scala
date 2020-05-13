package org.enso.languageserver.protocol.binary

import java.nio.ByteBuffer
import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import akka.http.scaladsl.model.RemoteAddress
import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.event.{
  DataSessionInitialized,
  DataSessionTerminated
}
import org.enso.languageserver.http.server.BinaryWebSocketControlProtocol.{
  ConnectionClosed,
  ConnectionFailed,
  OutboundStreamEstablished
}
import org.enso.languageserver.protocol.binary.InboundPayload.{
  INIT_SESSION_CMD,
  READ_FILE_CMD,
  WRITE_FILE_CMD
}
import org.enso.languageserver.protocol.binary.{
  EnsoUUID,
  InboundMessage,
  InitSessionCommand,
  OutboundPayload
}
import org.enso.languageserver.protocol.binary.BinaryConnectionController.InboundPayloadType
import org.enso.languageserver.protocol.binary.factory.{
  ErrorFactory,
  OutboundMessageFactory,
  SuccessReplyFactory,
  VisualisationUpdateFactory
}
import org.enso.languageserver.requesthandler.file.{
  ReadBinaryFileHandler,
  WriteBinaryFileHandler
}
import org.enso.languageserver.runtime.ContextRegistryProtocol.VisualisationUpdate
import org.enso.languageserver.session.BinarySession
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.util.binary.DecodingFailure
import org.enso.languageserver.util.binary.DecodingFailure.{
  DataCorrupted,
  EmptyPayload,
  GenericDecodingFailure
}

import scala.annotation.unused
import scala.concurrent.duration._

/**
  * An actor handling data communications between a single client and the
  * language server. It acts as a front controller responsible for handling
  * all incoming requests and dispatching commands.
  *
  * @param clientIp a client ip that the connection controller is created for
  */
class BinaryConnectionController(
  clientIp: RemoteAddress.IP,
  fileManager: ActorRef,
  requestTimeout: FiniteDuration = 10.seconds
) extends Actor
    with Stash
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive =
    connectionEndHandler() orElse connectionNotEstablished

  private def connectionNotEstablished: Receive = {
    case OutboundStreamEstablished(outboundChannel) =>
      unstashAll()
      context.become(
        connected(outboundChannel) orElse connectionEndHandler()
        orElse decodingFailureHandler(outboundChannel)
      )

    case _ => stash()
  }

  private def connected(outboundChannel: ActorRef): Receive = {
    case Right(msg: InboundMessage) if msg.payloadType() == INIT_SESSION_CMD =>
      val payload =
        msg.payload(new InitSessionCommand).asInstanceOf[InitSessionCommand]
      val clientId =
        new UUID(
          payload.identifier().mostSigBits(),
          payload.identifier().leastSigBits()
        )

      val responsePacket = createSessionInitResponsePacket(msg.messageId())
      outboundChannel ! responsePacket
      val session = BinarySession(clientId, self)
      context.system.eventStream.publish(DataSessionInitialized(session))
      log.info(s"Data session initialized for client: $clientId [$clientIp]")
      context.become(
        connectionEndHandler(Some(session))
        orElse initialized(
          outboundChannel,
          clientId,
          createRequestHandlers(outboundChannel)
        )
        orElse decodingFailureHandler(outboundChannel)
      )

  }

  private def initialized(
    outboundChannel: ActorRef,
    @unused clientId: UUID,
    handlers: Map[InboundPayloadType, Props]
  ): Receive = {
    case Right(msg: InboundMessage) =>
      if (handlers.contains(msg.payloadType())) {
        val handler = context.actorOf(handlers(msg.payloadType()))
        handler.forward(msg)
      } else {
        log.error(
          s"Received InboundMessage with unknown payload type: ${msg.payloadType()}"
        )
      }

    case update: VisualisationUpdate =>
      val updatePacket = convertVisualisationUpdateToOutPacket(update)
      outboundChannel ! updatePacket
  }

  private def connectionEndHandler(
    maybeDataSession: Option[BinarySession] = None
  ): Receive = {
    case ConnectionClosed =>
      log.info(s"Connection closed [$clientIp]")
      maybeDataSession.foreach(session =>
        context.system.eventStream.publish(DataSessionTerminated(session))
      )
      context.stop(self)

    case ConnectionFailed(th) =>
      log.error(
        s"An error occurred during processing web socket connection [$clientIp]",
        th
      )
      maybeDataSession.foreach(session =>
        context.system.eventStream.publish(DataSessionTerminated(session))
      )
      context.stop(self)
  }

  private def decodingFailureHandler(outboundChannel: ActorRef): Receive = {
    case Left(decodingFailure: DecodingFailure) =>
      val packet = convertDecodingFailureToOutPacket(decodingFailure)
      outboundChannel ! packet
  }

  private def convertDecodingFailureToOutPacket(
    decodingFailure: DecodingFailure
  ): ByteBuffer =
    decodingFailure match {
      case EmptyPayload  => ErrorFactory.createReceivedEmptyPayloadError()
      case DataCorrupted => ErrorFactory.createReceivedCorruptedDataError()
      case GenericDecodingFailure(th) =>
        log.error("Unrecognized error occurred in binary protocol", th)
        ErrorFactory.createServiceError()
    }

  private def convertVisualisationUpdateToOutPacket(
    update: VisualisationUpdate
  ): ByteBuffer = {
    implicit val builder = new FlatBufferBuilder(1024)
    val event            = VisualisationUpdateFactory.create(update)
    val msg = OutboundMessageFactory.create(
      UUID.randomUUID(),
      None,
      OutboundPayload.VISUALISATION_UPDATE,
      event
    )

    builder.finish(msg)
    val updatePacket = builder.dataBuffer()
    updatePacket
  }

  private def createSessionInitResponsePacket(
    requestId: EnsoUUID
  ): ByteBuffer = {
    implicit val builder = new FlatBufferBuilder(1024)
    val outMsg = OutboundMessageFactory.create(
      UUID.randomUUID(),
      Some(requestId),
      OutboundPayload.SUCCESS,
      SuccessReplyFactory.create()
    )
    builder.finish(outMsg)
    val responsePacket = builder.dataBuffer()
    responsePacket
  }

  private def createRequestHandlers(
    outboundChannel: ActorRef
  ): Map[InboundPayloadType, Props] = {
    Map(
      WRITE_FILE_CMD -> WriteBinaryFileHandler
        .props(requestTimeout, fileManager, outboundChannel),
      READ_FILE_CMD -> ReadBinaryFileHandler
        .props(requestTimeout, fileManager, outboundChannel)
    )
  }

}

object BinaryConnectionController {

  type InboundPayloadType = Byte

}
