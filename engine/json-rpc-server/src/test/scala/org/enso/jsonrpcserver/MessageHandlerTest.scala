package org.enso.jsonrpcserver
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import io.circe.literal._
import io.circe.parser._
import io.circe.{Decoder, Encoder, Json}
import org.enso.jsonrpcserver.MessageHandler.{Connected, WebMessage}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration._

class MessageHandlerTest
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  case object MyRequest extends Method("RequestMethod")
  case class MyRequestParams(foo: Int, bar: String)
      extends ParamsOf[MyRequest.type]
  case class MyRequestResult(baz: Int) extends ResultOf[MyRequest.type]

  case object MyNotification extends Method("NotificationMethod")
  case class MyNotificationParams(spam: String)
      extends ParamsOf[MyNotification.type]

  case object MyError extends Error(15, "Test error")

  object MyProtocol {
    import cats.syntax.functor._
    import io.circe.generic.auto._
    import io.circe.syntax._

    val encoder: Encoder[PayloadOf[Method]] = Encoder.instance {
      case m: MyRequestParams      => m.asJson
      case m: MyRequestResult      => m.asJson
      case m: MyNotificationParams => m.asJson
    }

    val protocol: Protocol =
      Protocol(
        Set(MyRequest, MyNotification),
        Map(
          MyNotification -> implicitly[Decoder[MyNotificationParams]].widen,
          MyRequest      -> implicitly[Decoder[MyRequestParams]].widen
        ),
        Map(
          MyRequest -> implicitly[Decoder[MyRequestResult]].widen
        ),
        Map(MyError.code -> MyError),
        encoder
      )
  }

  var out: TestProbe        = _
  var controller: TestProbe = _
  var handler: ActorRef     = _

  override def beforeAll(): Unit = {
    out        = TestProbe()
    controller = TestProbe()
    handler = system.actorOf(
      Props(new MessageHandler(MyProtocol.protocol, controller.ref))
    )
    handler ! Connected(out.ref)
  }

  "Message handler" must {

    "issue notifications" in {
      handler ! Notification(
        MyNotification,
        MyNotificationParams("test")
      )

      expectJson(out, json"""
          { "jsonrpc": "2.0",
            "method": "NotificationMethod",
            "params": { "spam": "test" }
          }""")
    }

    "receive notifications" in {
      handler ! WebMessage("""
                             |{ "jsonrpc": "2.0",
                             |  "method": "NotificationMethod",
                             |  "params": { "spam": "hello" }
                             |}
                             |""".stripMargin)

      controller.expectMsg(
        Notification(MyNotification, MyNotificationParams("hello"))
      )
    }

    "reply to requests" in {
      handler ! WebMessage("""
                             |{ "jsonrpc": "2.0",
                             |  "method": "RequestMethod",
                             |  "params": {"foo": 30, "bar": "bar"},
                             |  "id": "1234"
                             |}
                             |""".stripMargin)
      controller.expectMsg(
        Request(MyRequest, Id.String("1234"), MyRequestParams(30, "bar"))
      )
      controller.reply(
        ResponseResult(Some(Id.String("1234")), MyRequestResult(123))
      )

      expectJson(
        out,
        json"""
          { "jsonrpc": "2.0",
            "id": "1234",
            "result": {"baz": 123}
          }"""
      )
    }

    "reply with an error to malformed messages" in {
      handler ! WebMessage("Is this a JSON RPC message...?")
      expectJson(
        out,
        json"""
          { "jsonrpc": "2.0",
            "id": null,
            "error": { "code": -32700,
                       "message": "Parse error"
                     }
          }"""
      )
    }

    "reply with an error to unrecognized messages" in {
      handler ! WebMessage("""
                             |{ "jsonrpc": "2.0",
                             |  "method": "RequestMethodZZZZZ",
                             |  "params": {"foo": 30, "bar": "bar"},
                             |  "id": "1234"
                             |}
                             |""".stripMargin)

      expectJson(
        out,
        json"""
          { "jsonrpc": "2.0",
            "id": "1234",
            "error": { "code": -32601,
                       "message": "Method not found"
                     }
          }"""
      )
    }

    "reply with an error to messages with wrong params" in {
      handler ! WebMessage("""
                             |{ "jsonrpc": "2.0",
                             |  "method": "RequestMethod",
                             |  "params": {"foop": 30, "barp": "bar"},
                             |  "id": "1234"
                             |}
                             |""".stripMargin)

      expectJson(
        out,
        json"""
          { "jsonrpc": "2.0",
            "id": "1234",
            "error": { "code": -32602,
                       "message": "Invalid params"
                     }
          }"""
      )
    }

    "issue a request and pass a well formed response" in {
      handler ! Request(
        MyRequest,
        Id.String("some_id"),
        MyRequestParams(123, "456")
      )
      expectJson(
        out,
        json"""
          { "jsonrpc": "2.0",
            "method": "RequestMethod",
            "id": "some_id",
            "params": { "foo": 123,
                        "bar": "456" }
          }"""
      )
      handler ! WebMessage("""
                             |{ "jsonrpc": "2.0",
                             |  "id": "some_id",
                             |  "result": {"baz": 789}
                             |}
                             |""".stripMargin)
      controller.expectMsg(
        ResponseResult(Some(Id.String("some_id")), MyRequestResult(789))
      )
    }

    "issue a request and pass an error response" in {
      handler ! Request(
        MyRequest,
        Id.String("some_id"),
        MyRequestParams(123, "456")
      )
      expectJson(
        out,
        json"""
          { "jsonrpc": "2.0",
            "method": "RequestMethod",
            "id": "some_id",
            "params": { "foo": 123,
                        "bar": "456" }
          }"""
      )
      handler ! WebMessage("""
                             |{ "jsonrpc": "2.0",
                             |  "id": "some_id",
                             |  "error": { "code": 15,
                             |             "message": "Test error"
                             |           }
                             |}
                             |""".stripMargin)

      controller.expectMsg(ResponseError(Some(Id.String("some_id")), MyError))
    }
  }

  def expectJson(probe: TestProbe, expectedJson: Json): Unit = {
    val msg = probe.receiveOne(1.seconds)
    msg shouldBe an[WebMessage]
    val contents  = msg.asInstanceOf[WebMessage].message
    val maybeJson = parse(contents)
    maybeJson shouldBe Symbol("right")
    maybeJson.foreach(_ shouldEqual expectedJson)
  }
}
