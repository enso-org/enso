package org.enso.languageserver
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import io.circe.Json
import io.circe.literal._
import io.circe.parser._
import org.enso.languageserver.jsonrpc.MessageHandler.{Connected, WebMessage}
import org.enso.languageserver.jsonrpc.{
  Error,
  HasParams,
  HasResult,
  Id,
  MessageHandler,
  Method,
  Notification,
  Protocol,
  Request,
  ResponseError,
  ResponseResult,
  Unused
}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

import scala.concurrent.duration._

class MessageHandlerTest
    extends TestKit(ActorSystem("TestSystem"))
    with ImplicitSender
    with AnyWordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  case object MyRequest extends Method("RequestMethod") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = MyRequestParams
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = MyRequestResult
    }
  }
  case class MyRequestParams(foo: Int, bar: String)
  case class MyRequestResult(baz: Int)

  case object MyEmptyRequest extends Method("EmptyRequest") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }

  }

  case object MyNotification extends Method("NotificationMethod") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = MyNotificationParams
    }
  }
  case class MyNotificationParams(spam: String)

  case object MyEmptyNotification extends Method("EmptyNotification") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = Unused.type
    }
  }

  case object MyError extends Error(15, "Test error")

  object MyProtocol {
    import io.circe.generic.auto._

    val protocol: Protocol = Protocol.empty
      .registerNotification(MyNotification)
      .registerNotification(MyEmptyNotification)
      .registerRequest(MyRequest)
      .registerRequest(MyEmptyRequest)
      .registerError(MyError)
  }

  var out: TestProbe        = _
  var controller: TestProbe = _
  var handler: ActorRef     = _

  override def beforeEach(): Unit = {
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

    "receive notifications without params" in {
      handler ! WebMessage("""
                             |{ "jsonrpc": "2.0",
                             |  "method": "EmptyNotification"
                             |}
                             |""".stripMargin)

      controller.expectMsg(
        Notification(MyEmptyNotification, Unused)
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
        ResponseResult(MyRequest, Id.String("1234"), MyRequestResult(123))
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

    "reply to empty requests" in {
      handler ! WebMessage("""
                             |{ "jsonrpc": "2.0",
                             |  "method": "EmptyRequest",
                             |  "id": "1234"
                             |}
                             |""".stripMargin)
      controller.expectMsg(
        Request(MyEmptyRequest, Id.String("1234"), Unused)
      )
      controller.reply(
        ResponseResult(MyEmptyRequest, Id.String("1234"), Unused)
      )

      expectJson(
        out,
        json"""
          { "jsonrpc": "2.0",
            "id": "1234",
            "result": null
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
        ResponseResult(
          MyRequest,
          Id.String("some_id"),
          MyRequestResult(789)
        )
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
