package org.enso.languageserver.websocket.binary.factory

import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.InitSessionCommand
import org.enso.languageserver.protocol.binary.factory.EnsoUuidFactory

object SessionInitFactory {

  def create(clientId: UUID)(implicit builder: FlatBufferBuilder): Int = {
    InitSessionCommand.startInitSessionCommand(builder)
    val id = EnsoUuidFactory.create(clientId)
    InitSessionCommand.addIdentifier(builder, id)
    InitSessionCommand.endInitSessionCommand(builder)
  }

}
