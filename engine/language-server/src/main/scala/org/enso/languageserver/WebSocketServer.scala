package org.enso.languageserver

import java.util.UUID

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{Materializer, OverflowStrategy}
import org.enso.languageserver.jsonrpc.MessageHandler

import scala.concurrent.duration.{FiniteDuration, _}
import scala.concurrent.{ExecutionContext, Future}

object WebSocketServer {

  /**
    * A configuration object for properties of the WebSocketServer.
    *
    * @param outgoingBufferSize the number of messages buffered internally
    *                           if the downstream connection is lagging behind.
    * @param lazyMessageTimeout the timeout for downloading the whole of a lazy
    *                           stream message from the user.
    */
  case class Config(outgoingBufferSize: Int, lazyMessageTimeout: FiniteDuration)

  case object Config {

    /**
      * Creates a default instance of [[Config]].
      *
      * @return a default config.
      */
    def default: Config =
      Config(outgoingBufferSize = 10, lazyMessageTimeout = 10.seconds)
  }
}

/**
  * Exposes a multi-client Lanugage Server instance over WebSocket connections.
  * @param languageServer an instance of a running and initialized Language
  *                       Server.
  */
class WebSocketServer(
  languageServer: ActorRef,
  bufferRegistry: ActorRef,
  capabilityRouter: ActorRef,
  runtimeConnector: ActorRef,
  config: WebSocketServer.Config = WebSocketServer.Config.default
)(
  implicit val system: ActorSystem,
  implicit val materializer: Materializer
) {

  implicit val ec: ExecutionContext = system.dispatcher

  private val newConnectionPath: String = ""

  private def newUser(): Flow[Message, Message, NotUsed] = {
    val clientId = UUID.randomUUID()
    val clientActor =
      system.actorOf(
        Props(
          new ClientController(
            clientId,
            languageServer,
            bufferRegistry,
            capabilityRouter
          )
        )
      )

    val messageHandler =
      system.actorOf(
        Props(new MessageHandler(ClientApi.protocol, clientActor))
      )
    clientActor ! ClientApi.WebConnect(messageHandler)

    val incomingMessages: Sink[Message, NotUsed] =
      Flow[Message]
        .mapConcat({
          case textMsg: TextMessage => textMsg :: Nil
          case _: BinaryMessage     => Nil
        })
        .mapAsync(1)(
          _.toStrict(config.lazyMessageTimeout)
            .map(msg => MessageHandler.WebMessage(msg.text))
        )
        .to(
          Sink.actorRef[MessageHandler.WebMessage](
            messageHandler,
            MessageHandler.Disconnected, { _: Any =>
              MessageHandler.Disconnected
            }
          )
        )

    val outgoingMessages: Source[Message, NotUsed] =
      Source
        .actorRef[MessageHandler.WebMessage](
          PartialFunction.empty,
          PartialFunction.empty,
          config.outgoingBufferSize,
          OverflowStrategy.fail
        )
        .mapMaterializedValue { outActor =>
          messageHandler ! MessageHandler.Connected(outActor)
          NotUsed
        }
        .map(
          (outMsg: MessageHandler.WebMessage) => TextMessage(outMsg.message)
        )

    Flow.fromSinkAndSource(incomingMessages, outgoingMessages)
  }

  private val route: Route = path(newConnectionPath) {
    get { handleWebSocketMessages(newUser()) }
  }

  /**
    * Binds this server instance to a given port and interface, allowing
    * future connections.
    *
    * @param interface the interface to bind to.
    * @param port the port to bind to.
    * @return a server binding object.
    */
  def bind(interface: String, port: Int): Future[Http.ServerBinding] =
    Http().bindAndHandle(route, interface, port)
}
