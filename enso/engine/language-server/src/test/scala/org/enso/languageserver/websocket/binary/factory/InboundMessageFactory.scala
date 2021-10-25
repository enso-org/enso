package org.enso.languageserver.websocket.binary.factory

import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.{EnsoUUID, InboundMessage}
import org.enso.languageserver.protocol.binary.factory.EnsoUuidFactory

object InboundMessageFactory {

  def create(
    requestId: UUID,
    maybeCorrelationId: Option[EnsoUUID],
    payloadType: Byte,
    payload: Int
  )(implicit builder: FlatBufferBuilder): Int = {
    InboundMessage.startInboundMessage(builder)
    val reqId = EnsoUuidFactory.create(requestId)
    InboundMessage.addMessageId(builder, reqId)
    maybeCorrelationId.foreach { uuid =>
      val corId = EnsoUuidFactory.create(uuid)
      InboundMessage.addCorrelationId(builder, corId)
    }
    InboundMessage.addPayloadType(builder, payloadType)
    InboundMessage.addPayload(builder, payload)
    InboundMessage.endInboundMessage(builder)
  }

}
