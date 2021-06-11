package org.enso.languageserver.websocket.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.ReadBytesCommand

object ReadBytesCommandFactory {

  /** Creates a ReadBytes command.
   *
   * @param fileSegment the file segment to read bytes from
   * @param builder the flatbuffers builder
   * @return the offset of the ReadBytesCommand
   */
  def create(fileSegment: Int)(implicit builder: FlatBufferBuilder): Int = {
    ReadBytesCommand.createReadBytesCommand(builder, fileSegment)
  }

}
