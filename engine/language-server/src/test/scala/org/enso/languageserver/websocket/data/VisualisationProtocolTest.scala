package org.enso.languageserver.websocket.data

import java.nio.ByteBuffer
import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.data.envelope.{
  InboundPayload,
  OutboundMessage,
  OutboundPayload
}
import org.enso.languageserver.protocol.data.executioncontext
import org.enso.languageserver.runtime.ContextRegistryProtocol.{
  VisualisationContext,
  VisualisationUpdate
}
import org.enso.languageserver.websocket.data.factory.{
  InboundMessageFactory,
  SessionInitFactory
}
import org.scalatest.concurrent.Eventually

class VisualisationProtocolTest extends BaseBinaryServerTest with Eventually {

  implicit private val decoder = OutboundMessageDecoder

  "A visualisation binary protocol" must {

    "push visualisation updates when controller receives notification" in {
      //given
      val client = newWsClient()
      val data   = Array[Byte](1, 2, 3)
      val ctx = VisualisationContext(
        UUID.randomUUID(),
        UUID.randomUUID(),
        UUID.randomUUID()
      )
      client.send(createSessionInitCmd())
      client.expectFrame()
      //when
      lastConnectionController ! VisualisationUpdate(ctx, data)
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.VISUALISATION_UPDATE
      val payload = msg
        .payload(new executioncontext.VisualisationUpdate)
        .asInstanceOf[executioncontext.VisualisationUpdate]
      payload.dataAsByteBuffer().compareTo(ByteBuffer.wrap(data)) shouldBe 0
      payload
        .visualisationContext()
        .contextId()
        .leastSigBits() shouldBe ctx.contextId.getLeastSignificantBits
      payload
        .visualisationContext()
        .contextId()
        .mostSigBits() shouldBe ctx.contextId.getMostSignificantBits
      payload
        .visualisationContext()
        .expressionId()
        .leastSigBits() shouldBe ctx.expressionId.getLeastSignificantBits
      payload
        .visualisationContext()
        .expressionId()
        .mostSigBits() shouldBe ctx.expressionId.getMostSignificantBits
      payload
        .visualisationContext()
        .visualisationId()
        .leastSigBits() shouldBe ctx.visualisationId.getLeastSignificantBits
      payload
        .visualisationContext()
        .visualisationId()
        .mostSigBits() shouldBe ctx.visualisationId.getMostSignificantBits
    }

  }

  private def createSessionInitCmd(): ByteBuffer = {
    val clientId         = UUID.randomUUID()
    val requestId        = UUID.randomUUID()
    implicit val builder = new FlatBufferBuilder(1024)
    val cmd              = SessionInitFactory.create(clientId)
    val inMsg = InboundMessageFactory.create(
      requestId,
      None,
      InboundPayload.SESSION_INIT,
      cmd
    )
    builder.finish(inMsg)
    builder.dataBuffer()
  }

}
