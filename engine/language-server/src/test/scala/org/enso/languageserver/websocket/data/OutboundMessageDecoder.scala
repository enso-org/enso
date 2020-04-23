package org.enso.languageserver.websocket.data

import java.nio.ByteBuffer

import org.enso.languageserver.protocol.data.envelope.OutboundMessage
import org.enso.languageserver.util.binary.{BinaryDecoder, DecodingFailure}

object OutboundMessageDecoder extends BinaryDecoder[OutboundMessage] {

  override def decode(
    bytes: ByteBuffer
  ): Either[DecodingFailure, OutboundMessage] = {
    Right(OutboundMessage.getRootAsOutboundMessage(bytes))
  }

}
