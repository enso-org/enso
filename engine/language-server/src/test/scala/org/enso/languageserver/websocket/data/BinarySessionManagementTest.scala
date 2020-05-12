package org.enso.languageserver.websocket.data

import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.data.envelope.{
  InboundPayload,
  OutboundMessage,
  OutboundPayload
}
import org.enso.languageserver.websocket.data.factory.{
  InboundMessageFactory,
  SessionInitFactory
}

class BinarySessionManagementTest extends BaseBinaryServerTest {

  implicit private val decoder = OutboundMessageDecoder

  "Session Init cmd" must {

    "return empty SessionInitResponse" in {
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
