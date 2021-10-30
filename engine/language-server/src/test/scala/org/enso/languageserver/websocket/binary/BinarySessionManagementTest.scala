package org.enso.languageserver.websocket.binary

import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.{
  InboundPayload,
  OutboundMessage,
  OutboundPayload
}
import org.enso.languageserver.websocket.binary.factory.{
  InboundMessageFactory,
  SessionInitFactory
}
import org.enso.testkit.FlakySpec

class BinarySessionManagementTest extends BaseBinaryServerTest with FlakySpec {

  implicit private val decoder = OutboundMessageDecoder

  "Session Init cmd" must {

    "return empty SessionInitResponse" taggedAs Flaky in {
      //given
      val client           = newWsClient()
      val clientId         = UUID.randomUUID()
      val requestId        = UUID.randomUUID()
      implicit val builder = new FlatBufferBuilder(1024)
      val cmd              = SessionInitFactory.create(clientId)
      val inMsg = InboundMessageFactory.create(
        requestId,
        None,
        InboundPayload.INIT_SESSION_CMD,
        cmd
      )
      builder.finish(inMsg)
      //when
      client.send(builder.dataBuffer())
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.SUCCESS
      msg
        .correlationId()
        .leastSigBits() shouldBe requestId.getLeastSignificantBits
      msg
        .correlationId()
        .mostSigBits() shouldBe requestId.getMostSignificantBits
    }

  }

}
