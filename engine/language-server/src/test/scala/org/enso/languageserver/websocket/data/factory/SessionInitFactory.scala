package org.enso.languageserver.websocket.data.factory

import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.data.factory.EnsoUuidFactory
import org.enso.languageserver.protocol.data.session.InitSessionCommand

object SessionInitFactory {

  def create(clientId: UUID)(implicit builder: FlatBufferBuilder): Int = {
    InitSessionCommand.startInitSessionCommand(builder)
    val id = EnsoUuidFactory.create(clientId)
    InitSessionCommand.addIdentifier(builder, id)
    InitSessionCommand.endInitSessionCommand(builder)
  }

}
