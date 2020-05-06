package org.enso.languageserver.protocol.data

import java.nio.ByteBuffer
import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
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
import org.enso.languageserver.protocol.data.envelope.InboundPayload.SESSION_INIT
import org.enso.languageserver.protocol.data.envelope.{
  InboundMessage,
  OutboundPayload
}
import org.enso.languageserver.protocol.data.factory.{
  ErrorFactory,
  OutboundMessageFactory,
  SessionInitResponseFactory,
  VisualisationUpdateFactory
}
import org.enso.languageserver.protocol.data.session.SessionInit
import org.enso.languageserver.protocol.data.util.EnsoUUID
import org.enso.languageserver.runtime.ContextRegistryProtocol.VisualisationUpdate
import org.enso.languageserver.session.DataSession
import org.enso.languageserver.util.UnhandledLogging
import org.enso.languageserver.util.binary.DecodingFailure
import org.enso.languageserver.util.binary.DecodingFailure.{
  DataCorrupted,
  EmptyPayload,
  GenericDecodingFailure
}

import scala.annotation.unused

/**
  * An actor handling data communications between a single client and the
  * language server. It acts as a front controller responsible for handling
  * all incoming requests and dispatching commands.
  *
  * @param clientIp a client ip that the connection controller is created for
  */
class BinaryConnectionController(clientIp: RemoteAddress.IP)
    extends Actor
    with Stash
    with ActorLogging
    with UnhandledLogging {

  override def receive: Receive =
    connectionEndHandler() orElse connectionNotEstablished

  private def connectionNotEstablished: Receive = {
    case OutboundStreamEstablished(outboundChannel) =>
      log.info(s"Connection established [$clientIp]")
      unstashAll()
      context.become(
        connected(outboundChannel) orElse connectionEndHandler()
        orElse decodingFailureHandler(outboundChannel)
      )

    case _ => stash()
  }

  private def connected(outboundChannel: ActorRef): Receive = {
    case Right(msg: InboundMessage) if msg.payloadType() == SESSION_INIT =>
      val payload = msg.payload(new SessionInit).asInstanceOf[SessionInit]
      val clientId =
        new UUID(
          payload.identifier().mostSigBits(),
          payload.identifier().leastSigBits()
        )

      val responsePacket = createSessionInitResponsePacket(msg.requestId())
      outboundChannel ! responsePacket
      val session = DataSession(clientId, self)
      context.system.eventStream.publish(DataSessionInitialized(session))
      context.become(
        connectionEndHandler(Some(session))
        orElse initialized(outboundChannel, clientId)
        orElse decodingFailureHandler(outboundChannel)
      )

  }

  private def initialized(
    outboundChannel: ActorRef,
    @unused clientId: UUID
  ): Receive = {
    case update: VisualisationUpdate =>
      val updatePacket = convertVisualisationUpdateToOutPacket(update)
      outboundChannel ! updatePacket
  }

  private def connectionEndHandler(
    maybeDataSession: Option[DataSession] = None
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
  ): ByteBuffer = {
    implicit val builder = new FlatBufferBuilder(1024)
    val error =
      decodingFailure match {
        case EmptyPayload  => ErrorFactory.createReceivedEmptyPayloadError()
        case DataCorrupted => ErrorFactory.createReceivedCorruptedDataError()
        case GenericDecodingFailure(th) =>
          log.error("Unrecognized error occurred in binary protocol", th)
          ErrorFactory.createServiceError()
      }
    val outMsg = OutboundMessageFactory.create(
      UUID.randomUUID(),
      None,
      OutboundPayload.ERROR,
      error
    )
    builder.finish(outMsg)
    builder.dataBuffer()
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
      OutboundPayload.SESSION_INIT_RESPONSE,
      SessionInitResponseFactory.create()
    )
    builder.finish(outMsg)
    val responsePacket = builder.dataBuffer()
    responsePacket
  }

}
