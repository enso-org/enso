package org.enso.languageserver.websocket.binary

import java.nio.ByteBuffer
import java.util.UUID

import org.enso.languageserver.protocol.binary.{
  OutboundMessage,
  OutboundPayload,
  VisualisationUpdate => BinaryVisualisationUpdate
}
import org.enso.languageserver.runtime.ContextRegistryProtocol.{
  VisualisationContext,
  VisualisationUpdate
}
import org.enso.languageserver.util.binary.BinaryDecoder
import org.enso.testkit.FlakySpec
import org.scalatest.concurrent.Eventually

class VisualisationProtocolTest
    extends BaseBinaryServerTest
    with Eventually
    with FlakySpec {

  implicit private val decoder: BinaryDecoder[OutboundMessage] =
    OutboundMessageDecoder

  "A visualisation binary protocol" must {

    "push visualisation updates when controller receives notification" taggedAs Flaky in {
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
        .payload(new BinaryVisualisationUpdate)
        .asInstanceOf[BinaryVisualisationUpdate]
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

}
