package org.enso.languageserver.protocol.binary.factory

import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.OutboundMessage
import org.enso.languageserver.protocol.binary.EnsoUUID

object OutboundMessageFactory {

  /** Creates an [[OutboundMessage]] inside a [[FlatBufferBuilder]].
    *
    * @param messageId a unique id of the message sent from the server
    * @param maybeCorrelationId an optional correlation id used to correlate
    *                           a response with a request
    * @param payloadType a payload type indicating the type of the payload
    *                    piggybacked by the [[OutboundMessage]]
    * @param payload a message payload that carries requests sent by a client
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def create(
    messageId: UUID,
    maybeCorrelationId: Option[EnsoUUID],
    payloadType: Byte,
    payload: Int
  )(implicit builder: FlatBufferBuilder): Int = {
    OutboundMessage.startOutboundMessage(builder)
    val reqId = EnsoUuidFactory.create(messageId)
    OutboundMessage.addMessageId(builder, reqId)
    maybeCorrelationId.foreach { uuid =>
      val corId = EnsoUuidFactory.create(uuid)
      OutboundMessage.addCorrelationId(builder, corId)
    }
    OutboundMessage.addPayloadType(builder, payloadType)
    OutboundMessage.addPayload(builder, payload)
    OutboundMessage.endOutboundMessage(builder)
  }

}
