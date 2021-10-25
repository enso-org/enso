package org.enso.languageserver.websocket.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.ChecksumBytesCommand

object ChecksumBytesCommandFactory {

  /** Creates a new ChecksumBytesCommand.
    *
    * @param fileSegment the file segment to get the checksum of
    * @param builder the flat buffers builder
    * @return a new binary representation of a ChecksumBytesCommand.
    */
  def create(fileSegment: Int)(implicit builder: FlatBufferBuilder): Int = {
    ChecksumBytesCommand.createChecksumBytesCommand(builder, fileSegment)
  }
}
