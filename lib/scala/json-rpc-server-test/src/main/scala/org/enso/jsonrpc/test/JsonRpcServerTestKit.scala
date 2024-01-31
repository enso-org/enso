package org.enso.jsonrpc.test

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.testkit._
import io.circe.{ACursor, Decoder, DecodingFailure, HCursor, Json}
import io.circe.parser.parse
import org.enso.jsonrpc.{
  ClientControllerFactory,
  JsonRpcServer,
  ProtocolFactory
}
import org.scalactic.source.Position
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach, Inside}

import scala.concurrent.Await
import scala.concurrent.duration._

/** Test kit for testing JSON RPC servers.
  */
abstract class JsonRpcServerTestKit
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with FuzzyJsonMatchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Inside {

  implicit final class ACursorExpectedField(cursor: ACursor) {

    def downExpectedField(name: String): Decoder.Result[HCursor] = {
      val newCursor = cursor.downField(name)
      newCursor.success
        .toRight(DecodingFailure(s"Field '$name' not found", newCursor.history))
    }
  }

  override def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system, verifySystemShutdown = true)
    super.afterAll()
  }

  val interface       = "127.0.0.1"
  var address: String = _

  var server: JsonRpcServer       = _
  var binding: Http.ServerBinding = _

  def protocolFactory: ProtocolFactory

  def clientControllerFactory: ClientControllerFactory

  override def beforeEach(): Unit = {
    super.beforeEach()
    val factory = protocolFactory
    factory.init()
    server  = new JsonRpcServer(factory, clientControllerFactory)
    binding = Await.result(server.bind(interface, port = 0), 3.seconds)
    address = s"ws://$interface:${binding.localAddress.getPort}"
  }

  override def afterEach(): Unit = {
    Await.ready(binding.terminate(10.seconds.dilated), 15.seconds.dilated)
    super.afterEach()
  }

  class WsTestClient(address: String, debugMessages: Boolean = false) {
    private var inActor: ActorRef   = _
    private val outActor: TestProbe = TestProbe()
    private val source: Source[Message, NotUsed] = Source
      .actorRef[String](
        PartialFunction.empty,
        PartialFunction.empty,
        1,
        OverflowStrategy.fail
      )
      .mapMaterializedValue { actorRef =>
        inActor = actorRef
        NotUsed
      }
      .map { txt: String =>
        TextMessage(txt)
      }
    private val sink: Sink[Message, NotUsed] = Flow[Message]
      .map {
        case TextMessage.Strict(s) => s
        case _                     => throw new RuntimeException("Unexpected message type.")
      }
      .to(
        Sink.actorRef[String](
          outActor.ref,
          PoisonPill,
          { _: Any =>
            PoisonPill
          }
        )
      )
    private val flow = Flow.fromSinkAndSource(sink, source)

    Http()
      .singleWebSocketRequest(WebSocketRequest(address), flow)

    def send(message: String): Unit = {
      inActor ! message
    }

    def send(json: Json): Unit = send(json.noSpaces)

    def expectMessage(timeout: FiniteDuration = 5.seconds.dilated): String = {
      val message =
        try {
          outActor.expectMsgClass[String](timeout, classOf[String])
        } catch {
          case e: AssertionError if e.getMessage.contains("timeout") =>
            val sb = new StringBuilder(
              "Thread dump when timeout is reached while waiting for the message:\n"
            )
            Thread.getAllStackTraces.entrySet.forEach { entry =>
              sb.append(entry.getKey.getName).append("\n")
              entry.getValue.foreach { e =>
                sb.append("    ")
                  .append(e.getClassName)
                  .append(".")
                  .append(e.getMethodName)
                  .append("(")
                  .append(e.getFileName)
                  .append(":")
                  .append(e.getLineNumber)
                  .append(")\n")
              }
            }
            //println(sb.toString())
            throw e
        }
      if (debugMessages) println(message)
      message
    }

    def expectJson(
      json: Json,
      timeout: FiniteDuration = 5.seconds.dilated
    )(implicit pos: Position): Assertion = {
      val parsed = parse(expectMessage(timeout))
      parsed shouldEqual Right(json)
    }

    def expectSomeJson(
      timeout: FiniteDuration = 10.seconds.dilated
    )(implicit pos: Position): Json = {
      val parsed = parse(expectMessage(timeout))
      inside(parsed) { case Right(json) => json }
    }

    def fuzzyExpectJson(
      json: Json,
      timeout: FiniteDuration = 5.seconds.dilated
    ): Assertion = {
      val parsed = parse(expectMessage(timeout))

      parsed should fuzzyMatchJson(json)
    }

    def expectNoMessage(): Unit = outActor.expectNoMessage()

    def actorRef(): ActorRef = {
      outActor.ref
    }
  }
}

trait FuzzyJsonMatchers { self: Matchers =>
  class JsonEquals(expected: Json)
      extends Matcher[Either[io.circe.ParsingFailure, Json]] {
    val patch = inferPatch(expected)

    def apply(left: Either[io.circe.ParsingFailure, Json]) = {
      val leftFormatted     = patch[scala.util.Try](left.getOrElse(Json.Null))
      val expectedFormatted = patch[scala.util.Try](expected)
      MatchResult(
        leftFormatted == expectedFormatted,
        s"""Json $left did not equal "$expected"""",
        s"""Json $left is the same as "$expected""""
      )
    }

    def inferPatch(schema: Json): diffson.jsonpatch.JsonPatch[io.circe.Json] = {
      import diffson._
      import diffson.circe._
      import diffson.jsonpatch.simplediff._
      val schemaDiff = schema.toString().replaceAll("\\*", "\\?")
      diff(schema, parse(schemaDiff).getOrElse(Json.Null))
    }
  }

  def fuzzyMatchJson(expected: Json) = new JsonEquals(expected)
}
