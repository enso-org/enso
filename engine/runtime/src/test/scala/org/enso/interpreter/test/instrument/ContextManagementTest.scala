package org.enso.interpreter.test.instrument

import java.nio.ByteBuffer
import java.util.UUID

import org.enso.polyglot.runtime.Runtime.Api
import org.enso.polyglot.{LanguageInfo, RuntimeServerInfo}
import org.graalvm.polyglot.Context
import org.graalvm.polyglot.io.MessageEndpoint
import org.scalatest.BeforeAndAfterEach
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ContextManagementTest
    extends AnyFlatSpec
    with Matchers
    with BeforeAndAfterEach {

  var context: Context                 = _
  var messageQueue: List[Api.Response] = _
  var endPoint: MessageEndpoint        = _

  override protected def beforeEach(): Unit = {
    messageQueue = List()
    context = Context
      .newBuilder(LanguageInfo.ID)
      .allowExperimentalOptions(true)
      .option(RuntimeServerInfo.ENABLE_OPTION, "true")
      .serverTransport { (uri, peer) =>
        if (uri.toString == RuntimeServerInfo.URI) {
          endPoint = peer
          new MessageEndpoint {
            override def sendText(text: String): Unit = {}

            override def sendBinary(data: ByteBuffer): Unit =
              messageQueue ++= Api.deserializeResponse(data)

            override def sendPing(data: ByteBuffer): Unit = {}

            override def sendPong(data: ByteBuffer): Unit = {}

            override def sendClose(): Unit = {}
          }
        } else null
      }
      .build()
  }

  def send(msg: Api.Request): Unit = endPoint.sendBinary(Api.serialize(msg))
  def receive: Option[Api.Response] = {
    val msg = messageQueue.headOption
    messageQueue = messageQueue.drop(1)
    msg
  }

  "Runtime server" should "allow context creation and deletion" in {
    val requestId1 = UUID.randomUUID()
    val requestId2 = UUID.randomUUID()
    val contextId  = UUID.randomUUID()
    send(Api.Request(requestId1, Api.CreateContextRequest(contextId)))
    receive shouldEqual Some(
      Api.Response(requestId1, Api.CreateContextResponse(contextId))
    )
    send(Api.Request(requestId2, Api.DestroyContextRequest(contextId)))
    receive shouldEqual Some(
      Api.Response(requestId2, Api.DestroyContextResponse(contextId))
    )
  }

  "Runtime server" should "fail destroying a context if it does not exist" in {
    val requestId1 = UUID.randomUUID()
    val contextId1 = UUID.randomUUID()
    val requestId2 = UUID.randomUUID()
    val contextId2 = UUID.randomUUID()
    send(Api.Request(requestId1, Api.CreateContextRequest(contextId1)))
    receive shouldEqual Some(
      Api.Response(requestId1, Api.CreateContextResponse(contextId1))
    )
    send(Api.Request(requestId2, Api.DestroyContextRequest(contextId2)))
    receive shouldEqual Some(
      Api.Response(requestId2, Api.ContextNotExistError(contextId2))
    )
  }

  "Runtime server" should "push and pop the context stack" in {
    val contextId    = UUID.randomUUID()
    val expressionId = UUID.randomUUID()
    val requestId1   = UUID.randomUUID()
    val requestId2   = UUID.randomUUID()
    val requestId3   = UUID.randomUUID()
    send(Api.Request(requestId1, Api.CreateContextRequest(contextId)))
    receive shouldEqual Some(
      Api.Response(requestId1, Api.CreateContextResponse(contextId))
    )
    send(
      Api.Request(
        requestId2,
        Api.PushContextRequest(contextId, Api.StackItem.LocalCall(expressionId))
      )
    )
    receive shouldEqual Some(
      Api.Response(requestId2, Api.PushContextResponse(contextId))
    )
    send(Api.Request(requestId3, Api.PopContextRequest(contextId)))
    receive shouldEqual Some(
      Api.Response(requestId3, Api.PopContextResponse(contextId))
    )
  }

  "Runtime server" should "fail pushing context stack if it doesn't exist" in {
    val contextId    = UUID.randomUUID()
    val expressionId = UUID.randomUUID()
    val requestId    = UUID.randomUUID()
    send(
      Api.Request(
        requestId,
        Api.PushContextRequest(contextId, Api.StackItem.LocalCall(expressionId))
      )
    )
    receive shouldEqual Some(
      Api.Response(requestId, Api.ContextNotExistError(contextId))
    )
  }

  "Runtime server" should "fail popping empty stack" in {
    val contextId  = UUID.randomUUID()
    val requestId1 = UUID.randomUUID()
    val requestId2 = UUID.randomUUID()
    send(Api.Request(requestId1, Api.CreateContextRequest(contextId)))
    receive shouldEqual Some(
      Api.Response(requestId1, Api.CreateContextResponse(contextId))
    )
    send(Api.Request(requestId2, Api.PopContextRequest(contextId)))
    receive shouldEqual Some(
      Api.Response(requestId2, Api.EmptyStackError(contextId))
    )
  }
}
