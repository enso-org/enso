package org.enso.languageserver

import java.io.File
import java.util.UUID

import akka.NotUsed
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props, Stash}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage}
import akka.http.scaladsl.server.Directives._
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.stream.{ActorMaterializer, OverflowStrategy}
import org.enso.languageserver.jsonrpc.MessageHandler

import scala.concurrent.duration._
import scala.concurrent.Await
import scala.io.StdIn


object WebSocketServer {

  def main(args: Array[String]): Unit = {
    implicit val system       = ActorSystem()
    implicit val materializer = ActorMaterializer()

    val serverActor: ActorRef = system.actorOf(Props(new Server))

    serverActor ! LanguageProtocol.Initialize(
      LanguageProtocol.Config(List(), List())
    )

    def newUser(): Flow[Message, Message, NotUsed] = {
      val clientId    = UUID.randomUUID()
      val clientActor = system.actorOf(Props(new Client(clientId, serverActor)))

      val messageHandler =
        system.actorOf(
          Props(new MessageHandler(JsonRpcApi.protocol, clientActor))
        )
      clientActor ! JsonRpcApi.WsConnect(messageHandler)

      serverActor ! LanguageProtocol.Connect(
        clientId,
        LanguageProtocol
          .Client(clientActor, LanguageProtocol.Capabilities.default)
      )

      val incomingMessages: Sink[Message, NotUsed] =
        Flow[Message]
          .map {
            case TextMessage.Strict(text) =>
              MessageHandler.WebMessage(text)
          }
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
          .actorRef[MessageHandler.WebMessage](10, OverflowStrategy.fail)
          .mapMaterializedValue { outActor =>
            messageHandler ! MessageHandler.Connected(outActor)
            NotUsed
          }
          .map(
            (outMsg: MessageHandler.WebMessage) => TextMessage(outMsg.message)
          )

      Flow.fromSinkAndSource(incomingMessages, outgoingMessages)
    }

    val route =
      path("") {
        get {
          handleWebSocketMessages(newUser())
        }
      }

    val binding =
      Await.result(Http().bindAndHandle(route, "127.0.0.1", 1234), 3.seconds)

    // the rest of the sample code will go here
    println("Started server at 127.0.0.1:8080, press enter to kill server")
    StdIn.readLine()
    system.terminate()
  }
}
