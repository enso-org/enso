package org.enso.languageserver.protocol.binary.factory

import java.nio.ByteBuffer
import java.util.UUID
import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.{
  EnsoUUID,
  Error,
  ErrorPayload,
  OutboundPayload
}

object ErrorFactory {

  /** Creates a ReceivedCorruptedDataError inside a [[FlatBufferBuilder]].
    *
    * @return an FlatBuffer representation of the created error
    */
  def createReceivedCorruptedDataError(): ByteBuffer =
    createGenericError(1, "Received corrupted data")

  /** Creates a ReceivedEmptyPayloadError inside a [[FlatBufferBuilder]].
    *
    * @return an FlatBuffer representation of the created error
    */
  def createReceivedEmptyPayloadError(): ByteBuffer =
    createGenericError(2, "Received empty payload in the inbound message")

  /** Creates a ServiceError inside a [[FlatBufferBuilder]].
    *
    * @return an FlatBuffer representation of the created error
    */
  def createServiceError(
    maybeCorrelationId: Option[EnsoUUID] = None
  ): ByteBuffer =
    createGenericError(0, "Service error", maybeCorrelationId)

  /** Creates a generic error inside a [[FlatBufferBuilder]].
    *
    * @param code an error code
    * @param message an error textual message
    * @param maybeCorrelationId an optional correlation id used to correlate
    *                           a response with a request
    * @return an FlatBuffer representation of the created error
    */
  def createGenericError(
    code: Int,
    message: String,
    maybeCorrelationId: Option[EnsoUUID] = None
  ): ByteBuffer = {
    implicit val builder = new FlatBufferBuilder(1024)
    val offset =
      Error.createError(
        builder,
        code,
        builder.createString(message),
        ErrorPayload.NONE,
        0
      )
    val outMsg = OutboundMessageFactory.create(
      UUID.randomUUID(),
      maybeCorrelationId,
      OutboundPayload.ERROR,
      offset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

}
