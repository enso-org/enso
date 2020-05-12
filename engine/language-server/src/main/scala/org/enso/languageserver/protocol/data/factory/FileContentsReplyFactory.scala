package org.enso.languageserver.protocol.data.factory

import java.nio.ByteBuffer
import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.data.envelope.OutboundPayload
import org.enso.languageserver.protocol.data.filemanager.FileContentsReply
import org.enso.languageserver.protocol.data.util.EnsoUUID

object FileContentsReplyFactory {

  /**
    * Creates a [[FileContentsReply]] inside a [[FlatBufferBuilder]].
    *
    * @param contents the binary contents of a file
    * @param correlationId correlation id used to correlate a response with a
    *                      request
    * @return an FlatBuffer representation of the created error
    */
  def createPacket(
    contents: Array[Byte],
    correlationId: EnsoUUID
  ): ByteBuffer = {
    implicit val builder = new FlatBufferBuilder(1024)
    val contentsOffset   = builder.createByteVector(contents)
    val reply =
      FileContentsReply.createFileContentsReply(builder, contentsOffset)
    val outMsg = OutboundMessageFactory.create(
      UUID.randomUUID(),
      Some(correlationId),
      OutboundPayload.FILE_CONTENTS_REPLY,
      reply
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

}
