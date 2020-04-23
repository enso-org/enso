package org.enso.languageserver.http.server

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.http.scaladsl.model.{RemoteAddress, StatusCodes}
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{CompletionStrategy, Materializer, OverflowStrategy}
import akka.util.ByteString
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.http.server.BinaryWebSocketServer.Config
import org.enso.languageserver.http.server.BinaryWebSocketControlProtocol.{
  CloseConnection,
  ConnectionClosed,
  ConnectionFailed,
  OutboundStreamEstablished
}
import org.enso.languageserver.util.binary.{
  BinaryDecoder,
  BinaryEncoder,
  DecodingFailure
}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

/**
  * A web socket server using a binary protocol.
  *
  * @param decoder a decoder for inbound packets
  * @param encoder an encoder for outbound packets
  * @param factory creates front controller per a single connection that is
  *                responsible for handling all incoming requests
  * @param config a configuration object for properties of the server
  * @param system an actor system that hosts the server
  * @param materializer an actor materializer that converts inbound and outbound
  *                     flows into actors running these streams
  * @tparam A a type of messages sent to a connection controller
  * @tparam B a type of messages received from a connection controller
  */
class BinaryWebSocketServer[A, B](
  decoder: BinaryDecoder[A],
  encoder: BinaryEncoder[B],
  factory: ConnectionControllerFactory,
  config: Config = Config.default
)(
  implicit val system: ActorSystem,
  implicit val materializer: Materializer
) extends LazyLogging {

  implicit val ec: ExecutionContext = system.dispatcher

  private val route: Route =
    extractClientIP {
      case RemoteAddress.Unknown =>
        complete(
          InternalServerError -> "Set akka.http.server.remote-address-header " +
          "to on"
        )

      case ip: RemoteAddress.IP =>
        path(config.path) {
          get { handleWebSocketMessages(newConnection(ip)) }
        }
    }

  /**
    * Binds this server instance to a given port and interface, allowing
    * future connections.
    *
    * @param interface the interface to bind to.
    * @param port the port to bind to.
    * @return a representation of the binding state of the server.
    */
  def bind(interface: String, port: Int): Future[Http.ServerBinding] =
    Http().bindAndHandle(route, interface, port)

  private def newConnection(
    ip: RemoteAddress.IP
  ): Flow[Message, Message, NotUsed] = {

    val frontController = factory.createController(ip)

    val inboundFlow  = createInboundFlow(frontController, ip)
    val outboundFlow = createOutboundFlow(frontController)

    Flow.fromSinkAndSource(inboundFlow, outboundFlow)
  }

  private def createOutboundFlow(
    frontController: ActorRef
  ): Source[Message, NotUsed] = {
    Source
      .actorRef[B](
        completionMatcher,
        PartialFunction.empty,
        config.outgoingBufferSize,
        OverflowStrategy.fail
      )
      .mapMaterializedValue { outActor =>
        frontController ! OutboundStreamEstablished(outActor)
        NotUsed
      }
      .map { (outMsg: B) =>
        val bytes = encoder.encode(outMsg)
        BinaryMessage(ByteString.apply(bytes))
      }
  }

  private def completionMatcher: PartialFunction[Any, CompletionStrategy] = {
    case CloseConnection => CompletionStrategy.draining
  }

  private def createInboundFlow(
    frontController: ActorRef,
    ip: RemoteAddress.IP
  ): Sink[Message, NotUsed] = {
    Flow[Message]
      .mapConcat[BinaryMessage] {
        case msg: TextMessage =>
          logger.warn(
            s"Received text message $msg over the data connection [$ip]"
          )
          Nil

        case msg: BinaryMessage =>
          msg :: Nil
      }
      .mapAsync(Runtime.getRuntime.availableProcessors()) {
        _.toStrict(config.lazyMessageTimeout)
      }
      .map { binaryMsg =>
        val bytes = binaryMsg.data.asByteBuffer
        decoder.decode(bytes)
      }
      .to {
        Sink.actorRef[Either[DecodingFailure, A]](
          frontController,
          ConnectionClosed,
          ConnectionFailed
        )
      }
  }

}

object BinaryWebSocketServer {

  /**
    * A configuration object for properties of the [[BinaryWebSocketServer]].
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
    path: String = ""
  )

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
