package org.enso.languageserver.util.file

import java.util.UUID

import org.enso.languageserver.protocol.binary.EnsoUUID

object UuidUtils {

  /** Converts an [[EnsoUUID]] to a JVM [[UUID]].
    *
    * @param uuid a flatbuffers representation of uuid
    * @return [[UUID]]
    */
  def convertEnsoUuid(uuid: EnsoUUID): UUID =
    new UUID(uuid.mostSigBits(), uuid.leastSigBits())

}
