package org.enso.languageserver.protocol.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.{
  EnsoUUID,
  OutboundPayload,
  WriteBytesReply
}

import java.nio.ByteBuffer
import java.util.UUID

object WriteBytesReplyFactory {

  /** Creates a [[WriteBytesReply]] inside a [[FlatBufferBuilder]].
    *
    * @param checksum the checksum value of the written bytes
    * @param correlationId an identifier used to correlate a response with a
    *                      request
    * @return a FlatBuffer representation of the reply
    */
  def create(checksum: Array[Byte], correlationId: EnsoUUID): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(1024)

    val digestOffset = EnsoDigestFactory.create(checksum)
    val replyOffset =
      WriteBytesReply.createWriteBytesReply(builder, digestOffset)

    val outMsg = OutboundMessageFactory.create(
      UUID.randomUUID(),
      Some(correlationId),
      OutboundPayload.WRITE_BYTES_REPLY,
      replyOffset
    )

    builder.finish(outMsg)
    builder.dataBuffer()
  }
}
