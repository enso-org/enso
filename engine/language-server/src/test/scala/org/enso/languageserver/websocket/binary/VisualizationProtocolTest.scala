package org.enso.languageserver.websocket.binary

import java.nio.ByteBuffer
import java.util.UUID

import org.enso.languageserver.protocol.binary.{
  OutboundMessage,
  OutboundPayload,
  VisualizationUpdate => BinaryVisualizationUpdate
}
import org.enso.languageserver.runtime.ContextRegistryProtocol.{
  VisualizationContext,
  VisualizationUpdate
}
import org.enso.languageserver.util.binary.BinaryDecoder
import org.enso.testkit.FlakySpec
import org.scalatest.concurrent.Eventually

class VisualizationProtocolTest
    extends BaseBinaryServerTest
    with Eventually
    with FlakySpec {

  implicit private val decoder: BinaryDecoder[OutboundMessage] =
    OutboundMessageDecoder

  "A visualization binary protocol" must {

    "push visualization updates when controller receives notification" taggedAs Flaky in {
      //given
      val client = newWsClient()
      val data   = Array[Byte](1, 2, 3)
      val ctx = VisualizationContext(
        UUID.randomUUID(),
        UUID.randomUUID(),
        UUID.randomUUID()
      )
      client.send(createSessionInitCmd())
      client.expectFrame()
      //when
      lastConnectionController ! VisualizationUpdate(ctx, data)
      val Right(msg) = client.receiveMessage[OutboundMessage]()
      //then
      msg.payloadType() shouldBe OutboundPayload.VISUALIZATION_UPDATE
      val payload = msg
        .payload(new BinaryVisualizationUpdate)
        .asInstanceOf[BinaryVisualizationUpdate]
      payload.dataAsByteBuffer().compareTo(ByteBuffer.wrap(data)) shouldBe 0
      payload
        .visualizationContext()
        .contextId()
        .leastSigBits() shouldBe ctx.contextId.getLeastSignificantBits
      payload
        .visualizationContext()
        .contextId()
        .mostSigBits() shouldBe ctx.contextId.getMostSignificantBits
      payload
        .visualizationContext()
        .expressionId()
        .leastSigBits() shouldBe ctx.expressionId.getLeastSignificantBits
      payload
        .visualizationContext()
        .expressionId()
        .mostSigBits() shouldBe ctx.expressionId.getMostSignificantBits
      payload
        .visualizationContext()
        .visualizationId()
        .leastSigBits() shouldBe ctx.visualizationId.getLeastSignificantBits
      payload
        .visualizationContext()
        .visualizationId()
        .mostSigBits() shouldBe ctx.visualizationId.getMostSignificantBits
    }

  }

}
