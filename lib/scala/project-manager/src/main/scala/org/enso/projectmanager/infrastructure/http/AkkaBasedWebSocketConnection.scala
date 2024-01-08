package org.enso.projectmanager.infrastructure.http

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.http.scaladsl.{ConnectionContext, Http}
import akka.http.scaladsl.model.ws._
import akka.pattern.pipe
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{CompletionStrategy, OverflowStrategy}
import com.typesafe.scalalogging.Logger
import org.enso.jsonrpc.SecureConnectionConfig
import org.enso.projectmanager.infrastructure.http
import org.enso.projectmanager.infrastructure.http.AkkaBasedWebSocketConnection._
import org.enso.projectmanager.infrastructure.http.FanOutReceiver.{
  Attach,
  Detach
}
import org.enso.projectmanager.infrastructure.http.WebSocketConnection.{
  WebSocketConnected,
  WebSocketMessage,
  WebSocketStreamClosed,
  WebSocketStreamFailure
}

/** An Akka-based bidirectional web socket connection.
  *
  * @param address a server address
  * @param system an actor system
  */
class AkkaBasedWebSocketConnection(
  address: String,
  secureConfig: Option[SecureConnectionConfig]
)(implicit
  system: ActorSystem
) extends WebSocketConnection {

  private lazy val logger = Logger[http.AkkaBasedWebSocketConnection.type]

  import system.dispatcher

  private val receiver = system.actorOf(Props(new FanOutReceiver))

  private var outboundChannel: ActorRef = _

  private val source: Source[Message, NotUsed] = Source
    .actorRef[String](
      completionMatcher,
      PartialFunction.empty,
      1,
      OverflowStrategy.fail
    )
    .mapMaterializedValue { actorRef =>
      outboundChannel = actorRef
      NotUsed
    }
    .map { txt: String => TextMessage(txt) }

  private def completionMatcher: PartialFunction[Any, CompletionStrategy] = {
    case CloseWebSocket => CompletionStrategy.immediately
  }

  private val sink: Sink[Message, NotUsed] = Flow[Message]
    .map {
      case TextMessage.Strict(s) => WebSocketMessage(s)
      case _                     => throw new RuntimeException("Unmatched case")
    }
    .to(
      Sink.actorRef[WebSocketMessage](
        receiver,
        WebSocketStreamClosed,
        WebSocketStreamFailure
      )
    )

  private val flow = Flow.fromSinkAndSource(sink, source)

  /** @inheritdoc */
  override def attachListener(listener: ActorRef): Unit =
    receiver ! Attach(listener)

  /** @inheritdoc */
  override def detachListener(listener: ActorRef): Unit =
    receiver ! Detach(listener)

  /** @inheritdoc */
  override def connect(): Unit = {
    val server = Http()
    secureConfig
      .flatMap { config =>
        {
          val ctx = config
            .generateSSLContext()
            .map(sslContext => ConnectionContext.httpsClient(sslContext))
          if (ctx.isFailure) {
            logger.warn(
              "failed to establish requested secure context: {}",
              ctx.failed.get.getMessage
            )
          }
          ctx.toOption
        }
      }
      .foreach(ctx => server.setDefaultClientHttpsContext(ctx))
    val (future, _) =
      server
        .singleWebSocketRequest(
          WebSocketRequest(address),
          flow
        )
    future
      .map {
        case ValidUpgrade(_, _) =>
          WebSocketConnected

        case InvalidUpgradeResponse(_, cause) =>
          WebSocketStreamFailure(new Exception(s"Cannot connect $cause"))
      }
      .recover(WebSocketStreamFailure(_))
      .pipeTo(receiver)
    ()
  }

  /** @inheritdoc */
  override def send(message: String): Unit = {
    outboundChannel ! message
  }

  /** @inheritdoc */
  override def disconnect(): Unit = {
    outboundChannel ! CloseWebSocket
  }

  /** @inheritdoc */
  override def close(): Unit = {
    receiver ! PoisonPill
  }

}

object AkkaBasedWebSocketConnection {

  private object CloseWebSocket

}
