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

  var context: Context          = _
  var messageQueue: List[Api]   = _
  var endPoint: MessageEndpoint = _

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
              messageQueue ++= Api.deserialize(data)

            override def sendPing(data: ByteBuffer): Unit = {}

            override def sendPong(data: ByteBuffer): Unit = {}

            override def sendClose(): Unit = {}
          }
        } else null
      }
      .build()
  }

  def send(msg: Api): Unit = endPoint.sendBinary(Api.serialize(msg))
  def receive: Option[Api] = {
    val msg = messageQueue.headOption
    messageQueue = messageQueue.drop(1)
    msg
  }

  "Runtime server" should "allow context creation and deletion" in {
    val uuid = UUID.randomUUID()
    send(Api.CreateContextRequest(uuid))
    receive shouldEqual Some(Api.CreateContextResponse(uuid))
    send(Api.DestroyContextRequest(uuid))
    receive shouldEqual Some(Api.DestroyContextResponse(uuid, None))
  }

  "Runtime server" should "fail destroying a context if it does not exist" in {
    val uuid1 = UUID.randomUUID()
    val uuid2 = UUID.randomUUID()
    send(Api.CreateContextRequest(uuid1))
    receive shouldEqual Some(Api.CreateContextResponse(uuid1))
    send(Api.DestroyContextRequest(uuid2))
    receive shouldEqual Some(
      Api.DestroyContextResponse(uuid2, Some(Api.ContextDoesNotExistError()))
    )
  }
}
