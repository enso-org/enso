package org.enso.languageserver.http.server

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.http.server.BinaryWebSocketControlProtocol.OutboundStreamEstablished
import org.enso.languageserver.util.binary.{BinaryDecoder, BinaryEncoder}
import org.enso.ydoc.api.{MessageCallbacks, YjsChannel}

import java.nio.ByteBuffer

object BinaryYdocServer {

  /** A web socket server using a binary protocol.
    *
    * @param decoder a decoder for inbound packets
    * @param encoder an encoder for outbound packets
    * @param factory creates front controller per a single connection that is responsible for handling all incoming requests
    * @param messageCallbacks a list of message callbacks
    * @param system an actor system that hosts the server
    * @tparam A a type of messages sent to a connection controller
    * @tparam B a type of messages received from a connection controller
    */
  final class BinaryServerCallbacks[A, B](
    decoder: BinaryDecoder[A],
    encoder: BinaryEncoder[B],
    factory: ConnectionControllerFactory,
    messageCallbacks: List[ByteBuffer => Unit],
    system: ActorSystem
  ) extends MessageCallbacks
      with LazyLogging {

    override def onConnect(channel: YjsChannel): Unit = {
      logger.info("BinaryServerCallbacks.onConnect")

      val incomingMessageHandler = factory.createController()
      channel.subscribe(this.onMessage(incomingMessageHandler, _))

      val outgoingMessageHandler =
        system.actorOf(
          Props(new OutgoingMessageHandler(channel, encoder))
        )
      incomingMessageHandler ! OutboundStreamEstablished(outgoingMessageHandler)
    }

    private def onMessage(
      incomingMessageHandler: ActorRef,
      message: Object
    ): Unit = {
      logger.info(s"BinaryServerCallbacks.onMessage ${message.getClass}")
      message match {
        case bytes: ByteBuffer =>
          logger.info(s"Received binary message ${bytes.getClass}")
          decoder.decode(bytes) match {
            case Right(message) =>
              incomingMessageHandler ! message
            case Left(error) =>
              logger.error("Failed to decode binary message", error)
          }
          messageCallbacks.foreach(cb => cb(bytes))
        case _ =>
          logger.error(
            s"Received unsupported message: ${message.getClass}"
          )
      }
    }
  }

  final class OutgoingMessageHandler[B](
    channel: YjsChannel,
    encoder: BinaryEncoder[B]
  ) extends Actor
      with LazyLogging {

    override def receive: Receive = {
      case message: B @unchecked =>
        logger.info(s"Sending binary message $message")
        val bytes = encoder.encode(message)
        channel.send(bytes)
      case unknown =>
        logger.error(
          s"Sending unsupported message ${unknown.getClass}",
          unknown
        )
    }
  }
}
