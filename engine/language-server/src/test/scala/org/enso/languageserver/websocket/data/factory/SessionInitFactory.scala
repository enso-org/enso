package org.enso.languageserver.websocket.data.factory

import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.data.factory.EnsoUuidFactory
import org.enso.languageserver.protocol.data.session.SessionInit

object SessionInitFactory {

  def create(clientId: UUID)(implicit builder: FlatBufferBuilder): Int = {
    SessionInit.startSessionInit(builder)
    val id = EnsoUuidFactory.create(clientId)
    SessionInit.addIdentifier(builder, id)
    SessionInit.endSessionInit(builder)
  }

}
