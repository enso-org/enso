package org.enso.gateway

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import io.circe.Json
import org.enso.gateway.Server.Config
import org.enso.gateway.TestJson.{Initialize, WrongJsonrpc, WrongMethod}
import org.enso.{Gateway, LanguageServer}
import org.scalatest.{
  Assertion,
  AsyncFlatSpec,
  BeforeAndAfterAll,
  GivenWhenThen,
  Matchers
}
import io.circe.parser.parse

import scala.concurrent.Future

class GatewaySpec
    extends AsyncFlatSpec
    with Matchers
    with BeforeAndAfterAll
    with GivenWhenThen {
  implicit private val system: ActorSystem = ActorSystem()
  implicit private val materializer: ActorMaterializer =
    ActorMaterializer.create(system)

  import system.dispatcher

  override def beforeAll: Unit = {
    val languageServerActorName = "languageServer"
    val gatewayActorName        = "gateway"
    val languageServer: ActorRef =
      system.actorOf(LanguageServer.props(null), languageServerActorName)
    val gateway: ActorRef =
      system.actorOf(Gateway.props(languageServer), gatewayActorName)

    val jsonRpcController = new JsonRpcController(gateway)
    val server            = new Server(jsonRpcController)
    server.run()
  }

  override def afterAll: Unit = {
    system.terminate()
    ()
  }

  "Gateway" should "reply with a proper response to request with initialize method" in {
    checkRequestResponse(Initialize)
  }

  "Gateway" should "reply with a proper error to request with wrong jsonrpc" in {
    checkRequestResponse(WrongJsonrpc)
  }

  "Gateway" should "reply with a proper error to request with wrong method" in {
    checkRequestResponse(WrongMethod)
  }

  private def checkRequestResponse(
    testJsons: TestJson
  ): Future[Assertion] = {
    Given("server replies with responses to requests")
    val messageToMessageFlow: Flow[Message, Message, Future[Message]] =
      createFlow(
        TextMessage(testJsons.request.toString)
      )

    When("server receives request")
    val (_, messageFuture) = Http()
      .singleWebSocketRequest(
        WebSocketRequest(Config.addressString),
        messageToMessageFlow
      )

    Then("actual response server sent should correspond to expected")
    messageFuture.map {
      case message: TextMessage.Strict =>
        val actualResponse = parse(message.text).getOrElse(Json.Null)
        assert(actualResponse === testJsons.expectedResponse)
      case _ => assert(false, "binary or streamed text message")
    }
  }

  private def createFlow(
    textMessage: TextMessage.Strict
  ): Flow[Message, Message, Future[Message]] = {
    val source: Source[Message, NotUsed] =
      Source.single(textMessage)
    val sink = Sink.last[Message]
    Flow.fromSinkAndSourceMat(sink, source)(Keep.left)
  }
}
