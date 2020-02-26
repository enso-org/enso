package org.enso.languageserver

import java.nio.file.{Files, Paths}
import java.util.UUID

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{Message, TextMessage, WebSocketRequest}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import cats.effect.IO
import io.circe.Json
import io.circe.literal._
import io.circe.parser._
import org.enso.languageserver.data.Config
import org.enso.languageserver.filemanager.FileSystem
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach}

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.{Source => IoSource}

class WebSocketServerTest
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val interface = "127.0.0.1"
  val port      = 54321
  val address   = s"ws://$interface:$port"

  val testContentRoot   = Files.createTempDirectory(null)
  val testContentRootId = UUID.randomUUID()

  testContentRoot.toFile.deleteOnExit()

  var server: WebSocketServer     = _
  var binding: Http.ServerBinding = _

  override def beforeEach(): Unit = {
    val config = Config(Map(testContentRootId -> testContentRoot.toFile))
    val languageServer =
      system.actorOf(Props(new LanguageServer(config, new FileSystem[IO])))
    languageServer ! LanguageProtocol.Initialize
    server  = new WebSocketServer(languageServer)
    binding = Await.result(server.bind(interface, port), 3.seconds)
  }

  override def afterEach(): Unit = {
    val _ = binding.unbind()
  }

  "Language Server" must {
    "be able to grant and release capabilities" in {
      val probe        = new WsTestClient(address)
      val capabilityId = UUID.randomUUID()
      probe.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 1,
            "params": {
              "id": $capabilityId,
              "method": "canEdit",
              "registerOptions": { "path": "/Foo/bar" }
            }
          }
          """)
      probe.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)
      probe.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/release",
            "id": 2,
            "params": {
              "id": $capabilityId
            }
          }
          """)
      probe.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
    }

    "take canEdit capability away from clients when another client registers for it" in {
      val client1       = new WsTestClient(address)
      val client2       = new WsTestClient(address)
      val client3       = new WsTestClient(address)
      val capability1Id = UUID.randomUUID()
      val capability2Id = UUID.randomUUID()
      val capability3Id = UUID.randomUUID()

      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 1,
            "params": {
              "id": $capability1Id,
              "method": "canEdit",
              "registerOptions": { "path": "/Foo/bar" }
            }
          }
          """)

      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)
      client2.expectNoMessage()
      client3.expectNoMessage()

      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 2,
            "params": {
              "id": $capability2Id,
              "method": "canEdit",
              "registerOptions": { "path": "/Foo/bar" }
            }
          }
          """)

      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "capability/forceReleased",
            "params": {"id": $capability1Id}
          }
          """)
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
      client3.expectNoMessage()

      client3.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 3,
            "params": {
              "id": $capability3Id,
              "method": "canEdit",
              "registerOptions": { "path": "/Foo/bar" }
            }
          }
          """)

      client1.expectNoMessage()
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "method": "capability/forceReleased",
            "params": {"id": $capability2Id}
          }
          """)
      client3.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "result": null
          }
          """)
    }

    "implement the canEdit capability on a per-file basis" in {
      val client1       = new WsTestClient(address)
      val client2       = new WsTestClient(address)
      val capability1Id = UUID.randomUUID()
      val capability2Id = UUID.randomUUID()

      client1.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 1,
            "params": {
              "id": $capability1Id,
              "method": "canEdit",
              "registerOptions": { "path": "/Foo/bar" }
            }
          }
          """)

      client1.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 1,
            "result": null
          }
          """)
      client2.expectNoMessage()

      client2.send(json"""
          { "jsonrpc": "2.0",
            "method": "capability/acquire",
            "id": 2,
            "params": {
              "id": $capability2Id,
              "method": "canEdit",
              "registerOptions": { "path": "/Baz/spam" }
            }
          }
          """)

      client1.expectNoMessage()
      client2.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 2,
            "result": null
          }
          """)
    }

    "write textual content to a file" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 3,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo", "bar", "baz.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "result": null
          }
          """)
      client.expectNoMessage()
      val path = Paths.get(testContentRoot.toString, "foo", "bar", "baz.txt")
      IoSource.fromFile(path.toFile).getLines().mkString shouldBe "123456789"
    }

    "return failure when a content root cannot be found" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 3,
            "params": {
              "path": {
                "rootId": ${UUID.randomUUID()},
                "segments": [ "foo", "bar", "baz.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 3,
            "error" : {
              "code" : 1001,
              "message" : "Content root not found"
            }
          }
          """)
      client.expectNoMessage()
    }

    "read a file content" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/write",
            "id": 4,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              },
              "contents": "123456789"
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 4,
            "result": null
          }
          """)
      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/read",
            "id": 5,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "foo.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 5,
            "result": { "contents": "123456789" }
          }
          """)
    }

    "return FileNotFoundError if a file doesn't exist" in {
      val client = new WsTestClient(address)

      client.send(json"""
          { "jsonrpc": "2.0",
            "method": "file/read",
            "id": 6,
            "params": {
              "path": {
                "rootId": $testContentRootId,
                "segments": [ "bar.txt" ]
              }
            }
          }
          """)
      client.expectJson(json"""
          { "jsonrpc": "2.0",
            "id": 6,
            "error" : {
              "code" : 1003,
              "message" : "File not found"
            }
          }
          """)
    }

  }

  class WsTestClient(address: String) {
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
      }
      .to(Sink.actorRef[String](outActor.ref, PoisonPill, { _: Any =>
        PoisonPill
      }))
    private val flow = Flow.fromSinkAndSource(sink, source)

    Http()
      .singleWebSocketRequest(WebSocketRequest(address), flow)

    def send(message: String): Unit = {
      inActor ! message
    }

    def send(json: Json): Unit = send(json.noSpaces)

    def expectMessage(): String =
      outActor.expectMsgClass[String](classOf[String])

    def expectJson(json: Json): Assertion = {
      val parsed = parse(expectMessage())
      parsed shouldEqual Right(json)
    }

    def expectNoMessage(): Unit = outActor.expectNoMessage()
  }
}
