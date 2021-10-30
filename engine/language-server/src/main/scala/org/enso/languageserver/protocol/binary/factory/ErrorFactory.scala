package org.enso.languageserver.protocol.binary.factory

import java.nio.ByteBuffer
import java.util.UUID
import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.{
  EnsoUUID,
  Error,
  ErrorPayload,
  OutboundPayload,
  ReadOutOfBoundsError
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
    createGenericError(
      0,
      "Service error",
      maybeCorrelationId = maybeCorrelationId
    )

  /** Creates an error representing a read that is out of bounds in a file with
    * length `actualLength`.
    *
    * @param actualLength the actual length of the file
    * @param maybeCorrelationId an optional correlation ID for the error
    * @return a FlatBuffer representation of the error
    */
  def createReadOutOfBoundsError(
    actualLength: Long,
    maybeCorrelationId: Option[EnsoUUID] = None
  ): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(1024)

    val payloadData =
      ReadOutOfBoundsError.createReadOutOfBoundsError(builder, actualLength)

    createGenericErrorWithBuilder(
      1009,
      "Read is out of bounds for the file",
      Some(ErrorData(ErrorPayload.READ_OOB, payloadData)),
      maybeCorrelationId = maybeCorrelationId
    )
  }

  /** Creates a generic error inside a [[FlatBufferBuilder]].
    *
    * @param code an error code
    * @param message an error textual message
    * @param data optional error payload
    * @param maybeCorrelationId an optional correlation id used to correlate
    *                           a response with a request
    * @return an FlatBuffer representation of the created error
    */
  def createGenericError(
    code: Int,
    message: String,
    data: Option[ErrorData]              = None,
    maybeCorrelationId: Option[EnsoUUID] = None
  ): ByteBuffer = {
    implicit val builder: FlatBufferBuilder = new FlatBufferBuilder(1024)

    createGenericErrorWithBuilder(code, message, data, maybeCorrelationId)
  }

  /** Creates a generic error inside the provided [[FlatBufferBuilder]].
    *
    * @param code an error code
    * @param message an error textual message
    * @param data optional error payload
    * @param maybeCorrelationId an optional correlation id used to correlate
    *                           a response with a request
    * @param builder the builder to use for creating the error
    * @return a FlatBuffer representation of the created error
    */
  def createGenericErrorWithBuilder(
    code: Int,
    message: String,
    data: Option[ErrorData]              = None,
    maybeCorrelationId: Option[EnsoUUID] = None
  )(implicit builder: FlatBufferBuilder): ByteBuffer = {
    val offset = data match {
      case Some(d) =>
        Error.createError(
          builder,
          code,
          builder.createString(message),
          d.payloadVariant,
          d.payloadData
        )
      case None =>
        Error.createError(
          builder,
          code,
          builder.createString(message),
          ErrorPayload.NONE,
          0
        )
    }

    val outMsg = OutboundMessageFactory.create(
      UUID.randomUUID(),
      maybeCorrelationId,
      OutboundPayload.ERROR,
      offset
    )
    builder.finish(outMsg)
    builder.dataBuffer()
  }

  /** Stores additional data for the error.
    *
    * @param payloadVariant the variant set in the payload
    * @param payloadData the data for that variant
    */
  case class ErrorData(payloadVariant: Byte, payloadData: Int)
}
