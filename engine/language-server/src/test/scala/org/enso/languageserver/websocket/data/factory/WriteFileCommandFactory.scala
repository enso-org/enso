package org.enso.languageserver.websocket.data.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.data.filemanager.WriteFileCommand

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
