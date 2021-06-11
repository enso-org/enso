package org.enso.languageserver.protocol.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.EnsoDigest

object EnsoDigestFactory {

  /** Create a new EnsoDigest.
   *
   * @param bytes the bytes of the digest
   * @param builder the flatbuffer builder in which the digest is created
   * @return the offset of the digest in `builder`
   */
  def create(bytes: Array[Byte])(implicit builder: FlatBufferBuilder): Int = {
    val bytesOff = builder.createByteVector(bytes)
    EnsoDigest.createEnsoDigest(builder, bytesOff)
  }

}
