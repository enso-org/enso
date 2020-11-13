package org.enso.languageserver.protocol.binary.factory

import java.nio.ByteBuffer
import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.OutboundPayload
import org.enso.languageserver.protocol.binary.{EnsoUUID, Success}

object SuccessReplyFactory {

  /**
    * Creates a [[Success]] inside a [[FlatBufferBuilder]].
    *
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def create()(implicit builder: FlatBufferBuilder): Int = {
    Success.startSuccess(builder)
    Success.endSuccess(builder)
  }

  /**
    * Creates a [[Success]] inside a [[FlatBufferBuilder]].
    *
    * @param correlationId correlation id used to correlate a response with a
    *                      request
    * @return an FlatBuffer representation of the created error
    */
  def createPacket(correlationId: EnsoUUID): ByteBuffer = {
    implicit val builder = new FlatBufferBuilder(1024)
    val outMsg = OutboundMessageFactory.create(
      UUID.randomUUID(),
      Some(correlationId),
      OutboundPayload.SUCCESS,
      SuccessReplyFactory.create()
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

}
