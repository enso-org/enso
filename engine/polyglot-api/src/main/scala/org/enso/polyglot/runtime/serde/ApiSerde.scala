package org.enso.polyglot.runtime.serde

import org.enso.polyglot.runtime.Runtime.ApiEnvelope
import java.nio.ByteBuffer
import com.github.plokhotnyuk.jsoniter_scala.core.{
  readFromByteBuffer,
  writeToArray,
  JsonValueCodec
}

import org.enso.polyglot.macros.SerdeConfig
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker

import scala.util.Try

object ApiSerde {

  import SerdeConfig._

  implicit private val apiEnvelopeCodec: JsonValueCodec[ApiEnvelope] =
    JsonCodecMaker.make[ApiEnvelope](config)

  /** Serializes an ApiEnvelope into a byte buffer.
    *
    * @param message the message to serialize.
    * @return the serialized version of the message.
    */
  def serialize(message: ApiEnvelope): ByteBuffer = {
    ByteBuffer.wrap(writeToArray(message))
  }

  /** Deserializes a byte buffer into an ApiEnvelope, which can be a Request
    * or a Response.
    *
    * @param bytes the buffer to deserialize
    * @return the deserialized message, if the byte buffer can be deserialized.
    */
  def deserializeApiEnvelope(bytes: ByteBuffer): Try[ApiEnvelope] =
    Try(readFromByteBuffer[ApiEnvelope](bytes))
}
