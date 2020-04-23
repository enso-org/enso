package org.enso.languageserver.protocol.data

import java.nio.ByteBuffer

import org.enso.languageserver.protocol.data.envelope.{
  InboundMessage,
  InboundPayload
}
import org.enso.languageserver.util.binary.DecodingFailure.{
  DataCorrupted,
  EmptyPayload,
  GenericDecodingFailure
}
import org.enso.languageserver.util.binary.{BinaryDecoder, DecodingFailure}

/**
  * A decoder for an [[InboundMessage]].
  */
object InboundMessageDecoder extends BinaryDecoder[InboundMessage] {

  /** @inheritdoc **/
  override def decode(
    bytes: ByteBuffer
  ): Either[DecodingFailure, InboundMessage] =
    try {
      val inMsg = InboundMessage.getRootAsInboundMessage(bytes)
      if (inMsg.payloadType() == InboundPayload.NONE) {
        Left(EmptyPayload)
      } else {
        Right(inMsg)
      }
    } catch {
      case _: IndexOutOfBoundsException => Left(DataCorrupted)
      case throwable: Throwable         => Left(GenericDecodingFailure(throwable))
    }

}
