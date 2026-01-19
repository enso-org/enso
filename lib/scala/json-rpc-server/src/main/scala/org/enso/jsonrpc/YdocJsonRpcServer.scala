package org.enso.jsonrpc

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import com.typesafe.scalalogging.LazyLogging
import org.enso.jsonrpc.MessageHandler
import org.enso.ydoc.api.YjsChannelCallbacks
import org.enso.ydoc.api.YjsChannel

import java.util.UUID

import scala.concurrent.ExecutionContext

/** Exposes a multi-client JSON RPC Server instance over WebSocket connections.
  *
  * @param protocolFactory a protocol factory
  * @param clientControllerFactory a factory used to create a client controller
  * @param config a server config
  * @param optionalEndpoints a list of optional endpoints
  * @param system an actor system
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

  final class ServerCallbacks(
    protocolFactory: ProtocolFactory,
    clientControllerFactory: ClientControllerFactory,
    messageCallbacks: List[MessageHandler.WebMessage => Unit],
    system: ActorSystem
  ) extends YjsChannelCallbacks
      with LazyLogging {

    override def onConnect(channel: YjsChannel): Unit = {
      logger.info(s"ServerCallbacks.onConnect ${channel.getClass()}")

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
        channel.subscribe(this.onMessage(incomingMessageHandler, _))
      } catch {
        case e: Exception =>
          logger.error("ServerCallbacks.onConnect err", e)
      }

      val outgoingMessageHandler =
        system.actorOf(
          Props(
            new OutgoingMessageHandler(channel)
          )
        )
      incomingMessageHandler ! MessageHandler.Connected(outgoingMessageHandler)
    }

    private def onMessage(
      incomingMessageHandler: ActorRef,
      message: Object
    ): Unit = {
      message match {
        case m: String =>
          logger.info(s"Received message $m")
          val webMessage = MessageHandler.WebMessage(m)
          incomingMessageHandler ! webMessage
          messageCallbacks.foreach(cb => cb(webMessage))
        case _ =>
          logger.error("Received unsupported message:", message)
      }
    }
  }

  final class OutgoingMessageHandler(channel: YjsChannel) extends Actor with LazyLogging {

    override def receive: Receive = {
      case MessageHandler.WebMessage(message) =>
        logger.info(s"Sending message $message")
        channel.send(message)
      case unknown =>
        logger.error("Sending unsupported message:", unknown)
    }
  }
}
