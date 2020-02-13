package org.enso.gateway

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.scaladsl.{Flow, Keep, Sink, Source}
import io.circe.Json
import io.circe.parser.parse
import org.enso.{Gateway, LanguageServer}
import org.enso.gateway.TestJson.{
  Initialize,
  Shutdown,
  WrongJsonrpc,
  WrongMethod
}
import org.enso.gateway.server.Config
import org.scalatest.{Assertion, BeforeAndAfterAll, GivenWhenThen}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class GatewayJsonSpec
    extends AsyncFlatSpec
    with Matchers
    with BeforeAndAfterAll
    with GivenWhenThen {
  implicit private val system: ActorSystem = ActorSystem()

  import system.dispatcher

  private val languageServerActorName = "testingLanguageServer"
  private val gatewayActorName        = "testingGateway"
  private val languageServer: ActorRef =
    system.actorOf(LanguageServer.props(null), languageServerActorName)
  private val gateway: ActorRef =
    system.actorOf(Gateway.props(languageServer), gatewayActorName)

  private val jsonRpcController = new JsonRpcController(gateway)

  private val config = {
    val port = 30001
    val host = "localhost"
    new Config(port, host)
  }

  private val server = new Server(jsonRpcController, config)

  override def beforeAll: Unit = {
    server.run()
  }

  override def afterAll: Unit = {
    val terminationFuture = for {
      _ <- server.shutdown()
      _ <- system.terminate()
    } yield ()
    val timeout = 5.seconds
    Await.result(terminationFuture, timeout)
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

  "Gateway" should "reply with a proper response to request with shutdown method" in {
    checkRequestResponse(Shutdown)
  }

  private def checkRequestResponse(
    testJson: TestJson
  ): Future[Assertion] = {
    Given("server replies with responses to requests")
    val messageToMessageFlow: Flow[Message, Message, Future[Message]] =
      createFlow(
        TextMessage(testJson.request.toString)
      )

    When("server receives request")
    val (_, messageFuture) = Http()
      .singleWebSocketRequest(
        WebSocketRequest(config.addressString),
        messageToMessageFlow
      )

    Then("actual response server sent should correspond to expected")
    messageFuture.map {
      case message: TextMessage.Strict =>
        val actualResponse = parse(message.text).getOrElse(Json.Null)
        assert(actualResponse === testJson.expectedResponse)
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
