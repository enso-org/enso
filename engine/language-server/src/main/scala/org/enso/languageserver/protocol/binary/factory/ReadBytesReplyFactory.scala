package org.enso.languageserver.protocol.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.{
  EnsoUUID,
  OutboundPayload,
  ReadBytesReply
}

import java.nio.ByteBuffer
import java.util.UUID

object ReadBytesReplyFactory {

  /** Creates a [[ReadBytesReply]] inside a [[FlatBufferBuilder]].
    *
    * @param checksum the checksum value of the read bytes
    * @param bytes the bytes that were read
    * @param correlationId an identifier used to correlate a response with a
    *                      request
    * @return a FlatBuffer representation of the reply
    */
  def create(
    checksum: Array[Byte],
    bytes: Array[Byte],
    correlationId: EnsoUUID
  ): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(1024)

    val digestOffset = EnsoDigestFactory.create(checksum)
    val bytesOffset  = builder.createByteVector(bytes)
    val replyOffset =
      ReadBytesReply.createReadBytesReply(builder, digestOffset, bytesOffset)

    val outMsg = OutboundMessageFactory.create(
      UUID.randomUUID(),
      Some(correlationId),
      OutboundPayload.READ_BYTES_REPLY,
      replyOffset
    )

    builder.finish(outMsg)
    builder.dataBuffer()
  }
}
