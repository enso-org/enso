package org.enso.languageserver.http.server

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.http.server.BinaryWebSocketControlProtocol.OutboundStreamEstablished
import org.enso.languageserver.util.binary.{BinaryDecoder, BinaryEncoder}
import org.enso.ydoc.api.YjsChannel
import org.graalvm.polyglot.Context

import java.lang.foreign.MemorySegment
import java.nio.ByteBuffer

import scala.util.control.NonFatal

object BinaryYdocServer {

  /** Callbacks for binary protocol channels from the Ydoc server.
    *
    * When a WebSocket client connects to the Ydoc server requesting binary communication
    * (via the `data` query parameter), this callback sets up the message handling pipeline:
    * - Creates a connection controller to process incoming binary messages
    * - Subscribes to the channel with [[onMessage]] for inbound message decoding
    * - Creates [[OutgoingMessageHandler]] actor for encoding and sending responses
    *
    * Binary messages use direct ByteBuffer for efficient cross-runtime data transfer.
    *
    * @param decoder decodes inbound binary packets to typed messages
    * @param encoder encodes outbound typed messages to binary packets
    * @param factory creates connection controllers for each client session
    * @param messageCallbacks hooks invoked for each message (used for profiling)
    * @param context GraalVM polyglot context for converting JavaScript values
    * @param system the Akka actor system
    * @tparam A type of decoded inbound messages
    * @tparam B type of outbound messages to encode
    */
  final class BinaryServerCallbacks[A, B](
    decoder: BinaryDecoder[A],
    encoder: BinaryEncoder[B],
    factory: ConnectionControllerFactory,
    messageCallbacks: List[ByteBuffer => Unit],
    context: Context,
    system: ActorSystem
  ) extends YjsChannel.Server
      with LazyLogging {

    override def onConnect(channel: YjsChannel): Unit = {
      logger.trace(s"Binary channel connected ${channel.getClass()}")

      val incomingMessageHandler = factory.createController()
      channel.subscribe(this.onMessage(incomingMessageHandler, _))

      val outgoingMessageHandler =
        system.actorOf(
          Props(new OutgoingMessageHandler(channel, encoder))
        )
      incomingMessageHandler ! OutboundStreamEstablished(outgoingMessageHandler)
    }

    /** Decodes an incoming binary message and forwards to the controller. */
    private def onMessage(
      incomingMessageHandler: ActorRef,
      message: Object
    ): Unit = {
      logger.trace(s"Received binary message ${message.getClass}")
      try {
        val value   = context.asValue(message)
        val address = value.asNativePointer()
        val segment =
          MemorySegment.ofAddress(address).reinterpret(value.getBufferSize());
        val buffer  = segment.asByteBuffer()
        val decoded = decoder.decode(buffer)
        incomingMessageHandler ! decoded
        messageCallbacks.foreach(cb => cb(buffer))
      } catch {
        case NonFatal(e) =>
          logger.error(
            s"Received unsupported binary message: ${message.getClass}",
            e
          )
      }
    }
  }

  /** Actor that encodes and sends outgoing binary messages through the [[YjsChannel]]. */
  final class OutgoingMessageHandler[B](
    channel: YjsChannel,
    encoder: BinaryEncoder[B]
  ) extends Actor
      with LazyLogging {

    override def receive: Receive = {
      case message: B @unchecked =>
        logger.trace(s"Sending binary message $message")
        val bytes = encoder.encode(message)
        // Java `ByteBuffer` should be always compacted before sending because it will be
        // converted to JS `ArrayBuffer` assuming that it occupies the whole allocated size,
        // i.e. the conversion does not respect the position and limit attributes of `ByteBuffer`.
        // make it direct buffer as well
        val tmp = ByteBuffer.allocateDirect(bytes.remaining())
        tmp.put(0, bytes, bytes.position(), bytes.remaining())
        channel.send(tmp)
      case unknown =>
        logger.error(s"Sending unsupported message ${unknown.getClass}")
    }
  }
}
