package org.enso.projectmanager.infrastructure.http

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws._
import akka.pattern.pipe
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{CompletionStrategy, OverflowStrategy}
import org.enso.projectmanager.infrastructure.http.AkkaBasedWebSocketConnection._
import org.enso.projectmanager.infrastructure.http.FanOutReceiver.Listen
import org.enso.projectmanager.infrastructure.http.WebSocketConnection.{
  WebSocketConnected,
  WebSocketMessage,
  WebSocketStreamClosed,
  WebSocketStreamFailure
}

/**
  * An Akka-based bidirectional web socket connection.
  *
  * @param address a server address
  * @param system an actor system
  */
class AkkaBasedWebSocketConnection(address: String)(
  implicit system: ActorSystem
) extends WebSocketConnection {

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
    .map { txt: String =>
      TextMessage(txt)
    }

  private def completionMatcher: PartialFunction[Any, CompletionStrategy] = {
    case CloseWebSocket => CompletionStrategy.immediately
  }

  private val sink: Sink[Message, NotUsed] = Flow[Message]
    .map {
      case TextMessage.Strict(s) => WebSocketMessage(s)
      case _ => throw new RuntimeException("Unmatched case")
    }
    .to(
      Sink.actorRef[WebSocketMessage](
        receiver,
        WebSocketStreamClosed,
        WebSocketStreamFailure
      )
    )

  private val flow = Flow.fromSinkAndSource(sink, source)

  /** @inheritdoc **/
  override def attachListener(listener: ActorRef): Unit =
    receiver ! Listen(listener)

  /** @inheritdoc **/
  def connect(): Unit = {
    val (future, _) =
      Http()
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
      .pipeTo(receiver)
    ()
  }

  /** @inheritdoc **/
  def send(message: String): Unit = outboundChannel ! message

  /** @inheritdoc **/
  def disconnect(): Unit = outboundChannel ! CloseWebSocket

}

object AkkaBasedWebSocketConnection {

  private object CloseWebSocket

}
