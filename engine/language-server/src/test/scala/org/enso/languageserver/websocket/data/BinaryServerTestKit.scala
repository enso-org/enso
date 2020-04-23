package org.enso.languageserver.websocket.data

import java.nio.ByteBuffer

import akka.NotUsed
import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.ws.{BinaryMessage, Message, WebSocketRequest}
import akka.stream.OverflowStrategy
import akka.stream.scaladsl.{Flow, Sink, Source}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import akka.util.ByteString
import org.enso.languageserver.http.server.{
  BinaryWebSocketServer,
  ConnectionControllerFactory
}
import org.enso.languageserver.protocol.data.InboundMessageDecoder
import org.enso.languageserver.util.binary.{
  BinaryDecoder,
  BinaryEncoder,
  DecodingFailure
}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{Assertion, BeforeAndAfterAll, BeforeAndAfterEach}

import scala.concurrent.Await
import scala.concurrent.duration._

abstract class BinaryServerTestKit
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  val interface       = "127.0.0.1"
  var address: String = _

  var server: BinaryWebSocketServer[_, _] = _
  var binding: Http.ServerBinding         = _

  def connectionControllerFactory: ConnectionControllerFactory

  override def beforeEach(): Unit = {
    server = new BinaryWebSocketServer(
      InboundMessageDecoder,
      BinaryEncoder.empty,
      connectionControllerFactory
    )
    binding = Await.result(server.bind(interface, port = 0), 3.seconds)
    address = s"ws://$interface:${binding.localAddress.getPort}"
  }

  override def afterEach(): Unit = {
    val _ = binding.unbind()
  }

  protected def newWsClient(): WsTestClient = new WsTestClient(address)

  class WsTestClient(address: String) {
    private var inActor: ActorRef   = _
    private val outActor: TestProbe = TestProbe()
    private val source: Source[Message, NotUsed] = Source
      .actorRef[ByteBuffer](
        PartialFunction.empty,
        PartialFunction.empty,
        1,
        OverflowStrategy.fail
      )
      .mapMaterializedValue { actorRef =>
        inActor = actorRef
        NotUsed
      }
      .map { frame: ByteBuffer => BinaryMessage(ByteString(frame)) }

    private val sink: Sink[Message, NotUsed] = Flow[Message]
      .collect {
        case BinaryMessage.Strict(data) => data.asByteBuffer
      }
      .to(Sink.actorRef[ByteBuffer](outActor.ref, PoisonPill, { _: Any =>
        PoisonPill
      }))

    private val flow = Flow.fromSinkAndSource(sink, source)

    Http()
      .singleWebSocketRequest(WebSocketRequest(address), flow)

    def send(frame: ByteBuffer): Unit = {
      inActor ! frame
    }

    def expectFrame(): ByteBuffer =
      outActor.expectMsgClass[ByteBuffer](classOf[ByteBuffer])

    def receiveMessage[T: BinaryDecoder](): Either[DecodingFailure, T] = {
      val frame = expectFrame()
      implicitly[BinaryDecoder[T]].decode(frame)
    }

    def expectMessage[T: BinaryDecoder](msg: T): Assertion = {
      val frame   = expectFrame()
      val decoded = implicitly[BinaryDecoder[T]].decode(frame)
      decoded shouldEqual msg
    }

    def expectNoMessage(): Unit = outActor.expectNoMessage()

  }

}
