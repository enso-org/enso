package org.enso.languageserver.websocket.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.WriteBytesCommand

object WriteBytesCommandFactory {

  /** Creates a WriteBytes command.
    *
    * @param path the path to the file into which bytes should be written
    * @param byteOffset the byte offset at which to start writing
    * @param overwriteExisting whether or not existing bytes should be
    *                          overwritten
    * @param bytes the bytes to be written
    * @param builder the FlatBuffers builder
    * @return the offset of the WriteBytesCommand.
    */
  def create(
    path: Int,
    byteOffset: Long,
    overwriteExisting: Boolean,
    bytes: Array[Byte]
  )(implicit builder: FlatBufferBuilder): Int = {
    val bytesOff = builder.createByteVector(bytes)
    WriteBytesCommand.createWriteBytesCommand(
      builder,
      path,
      byteOffset,
      overwriteExisting,
      bytesOff
    )
  }
}
