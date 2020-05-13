package org.enso.languageserver.websocket.binary.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.binary.WriteFileCommand

object WriteFileCommandFactory {

  def create(
    path: Int,
    contents: Array[Byte]
  )(implicit builder: FlatBufferBuilder): Int = {
    val contentsOffset = builder.createByteVector(contents)
    WriteFileCommand.startWriteFileCommand(builder)
    WriteFileCommand.addPath(builder, path)
    WriteFileCommand.addContents(builder, contentsOffset)
    WriteFileCommand.endWriteFileCommand(builder)
  }

}
