package org.enso.jsonrpc

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{Materializer, OverflowStrategy}
import com.typesafe.scalalogging.LazyLogging

import java.util.UUID
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext

/** Exposes a multi-client JSON RPC Server instance over WebSocket connections.
  *
  * @param protocolFactory a protocol factory
  * @param clientControllerFactory a factory used to create a client controller
  * @param config a server config
  * @param optionalEndpoints a list of optional endpoints
  * @param system an actor system
  * @param materializer a materializer
  */
class JsonRpcServer(
  protocolFactory: ProtocolFactory,
  clientControllerFactory: ClientControllerFactory,
  config: JsonRpcServer.Config      = JsonRpcServer.Config.default,
  optionalEndpoints: List[Endpoint] = List.empty
)(
  implicit val system: ActorSystem,
  implicit val materializer: Materializer
) extends Server
    with LazyLogging {

  implicit val ec: ExecutionContext = system.dispatcher

  private def newUser(port: Int): Flow[Message, Message, NotUsed] = {
    val messageHandler =
      system.actorOf(
        Props(
          new MessageHandlerSupervisor(
            clientControllerFactory,
            protocolFactory,
            port
          )
        ),
        s"message-handler-supervisor-${UUID.randomUUID()}"
      )

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
        .wireTap { webMessage =>
          logger.trace(s"Received text message: ${webMessage.message}.")
        }
        .to(
          Sink.actorRef[MessageHandler.WebMessage](
            messageHandler,
            MessageHandler.Disconnected(port),
            { _: Throwable =>
              MessageHandler.Disconnected(port)
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
          messageHandler ! MessageHandler.Connected(outActor, port)
          NotUsed
        }
        .map((outMsg: MessageHandler.WebMessage) => TextMessage(outMsg.message))
        .wireTap { textMessage =>
          logger.trace(s"Sent text message ${textMessage.text}.")
        }

    Flow.fromSinkAndSource(incomingMessages, outgoingMessages)
  }

  override protected def serverRoute(port: Int): Route = {
    val webSocketEndpoint =
      path(config.path) {
        get { handleWebSocketMessages(newUser(port)) }
      }

    optionalEndpoints.foldLeft(webSocketEndpoint) { (chain, next) =>
      chain ~ next.route
    }
  }

  override protected def secureConfig(): Option[SecureConnectionConfig] =
    config.secureConfig
}

object JsonRpcServer {

  /** A configuration object for properties of the JsonRpcServer.
    *
    * @param outgoingBufferSize the number of messages buffered internally
    *                           if the downstream connection is lagging behind.
    * @param lazyMessageTimeout the timeout for downloading the whole of a lazy
    *                           stream message from the user.
    * @param path the http path that the server listen to.
    */
  case class Config(
    outgoingBufferSize: Int,
    lazyMessageTimeout: FiniteDuration,
    secureConfig: Option[SecureConnectionConfig],
    path: String = ""
  )

  case object Config {

    /** Creates a default instance of [[Config]].
      *
      * @return a default config.
      */
    def default: Config =
      Config(
        outgoingBufferSize = 1000,
        lazyMessageTimeout = 10.seconds,
        secureConfig       = None
      )
  }

  case class WebConnect(webActor: ActorRef, port: Int)

}
