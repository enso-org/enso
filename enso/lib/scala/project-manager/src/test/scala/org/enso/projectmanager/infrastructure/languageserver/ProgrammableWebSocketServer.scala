package org.enso.projectmanager.infrastructure.languageserver

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, TextMessage}
import akka.http.scaladsl.server.Directives.{handleWebSocketMessages, path}
import akka.stream.scaladsl.{Flow, Sink}
import akka.testkit._
import org.enso.projectmanager.infrastructure.languageserver.ProgrammableWebSocketServer.{
  Behaviour,
  Reject,
  ReplyWith
}

import scala.concurrent.Await
import scala.concurrent.duration._

class ProgrammableWebSocketServer(interface: String, port: Int)(implicit
  system: ActorSystem
) {

  @volatile
  private var behaviour: Behaviour = (_ => Reject)

  @volatile
  private var maybeBinding: Option[Http.ServerBinding] = None

  def withBehaviour(f: Behaviour): Unit = {
    this.behaviour = f
  }

  private val handler: Flow[Message, Message, Any] =
    Flow[Message].mapConcat {
      case tm: TextMessage =>
        val payload =
          Await.result(tm.toStrict(3.seconds.dilated), 3.seconds.dilated).text
        if (behaviour.isDefinedAt(payload)) {
          behaviour.apply(payload) match {
            case Reject           => Nil
            case ReplyWith(reply) => TextMessage(reply) :: Nil
          }
        } else {
          Nil
        }

      case bm: BinaryMessage =>
        bm.dataStream.runWith(Sink.ignore)
        Nil
    }

  private val websocketRoute =
    path("") {
      handleWebSocketMessages(handler)
    }

  def start(): Unit = {
    val binding =
      Await.result(
        Http().bindAndHandle(websocketRoute, interface, port),
        3.seconds.dilated
      )
    maybeBinding = Some(binding)
  }

  def stop(): Unit =
    maybeBinding.foreach { binding =>
      Await.result(binding.unbind(), 5.seconds.dilated)
    }

}

object ProgrammableWebSocketServer {

  type Behaviour = PartialFunction[String, Response]

  sealed trait Response
  case object Reject                  extends Response
  case class ReplyWith(reply: String) extends Response

}
