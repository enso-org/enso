package org.enso.languageserver.protocol.binary.factory

import java.util.UUID

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.EnsoUUID

object EnsoUuidFactory {

  /** Creates EnsoUUID inside a [[FlatBufferBuilder]].
    *
    * @param uuid a uuid to serialize
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def create(uuid: UUID)(implicit builder: FlatBufferBuilder): Int = {
    EnsoUUID.createEnsoUUID(
      builder,
      uuid.getLeastSignificantBits,
      uuid.getMostSignificantBits
    )
  }

  /** Creates an [[EnsoUUID]] inside a [[FlatBufferBuilder]].
    *
    * @param uuid a uuid to serialize
    * @param builder a class that helps build a FlatBuffer representation of
    *                complex objects
    * @return an offset pointing to the FlatBuffer representation of the
    *         created object
    */
  def create(uuid: EnsoUUID)(implicit builder: FlatBufferBuilder): Int = {
    EnsoUUID.createEnsoUUID(
      builder,
      uuid.leastSigBits(),
      uuid.mostSigBits()
    )
  }

}
