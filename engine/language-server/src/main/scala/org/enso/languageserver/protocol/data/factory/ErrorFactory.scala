package org.enso.languageserver.protocol.data.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.data.util.Error

object ErrorFactory {

  /**
    * Creates a ReceivedCorruptedDataError inside a [[FlatBufferBuilder]].
    *
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def createReceivedCorruptedDataError()(
    implicit builder: FlatBufferBuilder
  ): Int =
    Error.createError(
      builder,
      1,
      builder.createString("Received corrupted data")
    )

  /**
    * Creates a ReceivedEmptyPayloadError inside a [[FlatBufferBuilder]].
    *
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def createReceivedEmptyPayloadError()(
    implicit builder: FlatBufferBuilder
  ): Int =
    Error.createError(
      builder,
      2,
      builder.createString("Received empty payload in the inbound message")
    )

  /**
    * Creates a ServiceError inside a [[FlatBufferBuilder]].
    *
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def createServiceError()(
    implicit builder: FlatBufferBuilder
  ): Int =
    Error.createError(
      builder,
      0,
      builder.createString("Service error")
    )

}
