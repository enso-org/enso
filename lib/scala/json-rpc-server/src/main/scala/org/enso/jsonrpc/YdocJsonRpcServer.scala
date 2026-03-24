package org.enso.jsonrpc

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.MessageHandler
import org.enso.ydoc.api.YjsChannel

import java.util.UUID
import java.util.function.Consumer

import scala.concurrent.ExecutionContext

/** JSON-RPC server that communicates with the Ydoc server via [[YjsChannel]].
  *
  * This server receives JSON-RPC messages from the Ydoc server (which handles WebSocket
  * connections from IDE clients) and processes them using the standard JSON-RPC protocol
  * infrastructure. The [[yjsChannelCallbacks]] is passed to the Ydoc server during startup.
  *
  * @param protocolFactory creates protocol instances for message parsing
  * @param clientControllerFactory creates controllers for handling client sessions
  * @param config server configuration
  * @param optionalEndpoints additional HTTP endpoints (health check, idleness, etc.)
  * @param messageCallbacks hooks invoked for each message (used for profiling)
  * @param system the Akka actor system
  */
class YdocJsonRpcServer(
  protocolFactory: ProtocolFactory,
  clientControllerFactory: ClientControllerFactory,
  config: JsonRpcServer.Config                              = JsonRpcServer.Config.default,
  optionalEndpoints: List[Endpoint]                         = List.empty,
  messageCallbacks: List[MessageHandler.WebMessage => Unit] = List.empty
)(implicit
  val system: ActorSystem
) extends Server
    with LazyLogging {

  implicit val ec: ExecutionContext = system.dispatcher

  val yjsChannelCallbacks =
    new YdocJsonRpcServer.ServerCallbacks(
      protocolFactory,
      clientControllerFactory,
      messageCallbacks,
      system
    )

  override protected def serverRoute(port: Int): Route = {
    val emptyEndpoint =
      path("__null") {
        post { null }
      }

    optionalEndpoints.foldLeft(emptyEndpoint) { (chain, next) =>
      chain ~ next.route
    }
  }

  override protected def secureConfig(): Option[SecureConnectionConfig] =
    config.secureConfig
}

object YdocJsonRpcServer {

  /** Callbacks for JSON-RPC channels from the Ydoc server.
    *
    * When a WebSocket client connects to the Ydoc server requesting JSON-RPC communication,
    * this callback sets up the message handling pipeline:
    * - Creates a [[MessageHandlerSupervisor]] actor to process incoming messages
    * - Subscribes [[OnMessageHandler]] to the channel for inbound messages
    * - Creates [[OutgoingMessageHandler]] actor for sending responses via the channel
    */
  final class ServerCallbacks(
    protocolFactory: ProtocolFactory,
    clientControllerFactory: ClientControllerFactory,
    messageCallbacks: List[MessageHandler.WebMessage => Unit],
    system: ActorSystem
  ) extends YjsChannel.Server
      with LazyLogging {

    override def onConnect(channel: YjsChannel): Unit = {
      logger.trace(s"JSON-RPC channel connected ${channel.getClass()}")

      val incomingMessageHandler =
        system.actorOf(
          Props(
            new MessageHandlerSupervisor(
              clientControllerFactory,
              protocolFactory
            )
          ),
          s"message-handler-supervisor-${UUID.randomUUID()}"
        )
      try {
        val toSubscribe =
          new OnMessageHandler(messageCallbacks, incomingMessageHandler)
        channel.subscribe(toSubscribe)
      } catch {
        case e: Exception =>
          logger.error("JSON-RPC channel subscribe error", e)
      }

      val outgoingMessageHandler =
        system.actorOf(
          Props(
            new OutgoingMessageHandler(channel)
          )
        )
      incomingMessageHandler ! MessageHandler.Connected(outgoingMessageHandler)
    }
  }

  /** Handles incoming JSON-RPC messages from the [[YjsChannel]].
    *
    * Converts raw String messages to [[MessageHandler.WebMessage]] and forwards them
    * to the message handler actor. Also invokes any registered message callbacks.
    */
  final class OnMessageHandler(
    messageCallbacks: List[MessageHandler.WebMessage => Unit],
    incomingMessageHandler: ActorRef
  ) extends Consumer[Object]
      with LazyLogging {

    override def accept(message: Object): Unit = {
      message match {
        case m: String =>
          logger.trace(s"Received JSON-RPC message $m")
          val webMessage = MessageHandler.WebMessage(m)
          incomingMessageHandler ! webMessage
          messageCallbacks.foreach(cb => cb(webMessage))
        case _ =>
          logger.error("Received unsupported JSON-RPC message:", message)
      }
    }
  }

  /** Actor that sends outgoing JSON-RPC messages through the [[YjsChannel]]. */
  final class OutgoingMessageHandler(channel: YjsChannel)
      extends Actor
      with LazyLogging {

    override def receive: Receive = {
      case MessageHandler.WebMessage(message) =>
        logger.trace(s"Sending message $message")
        channel.send(message)
      case unknown =>
        logger.error("Sending unsupported message:", unknown)
    }
  }
}
