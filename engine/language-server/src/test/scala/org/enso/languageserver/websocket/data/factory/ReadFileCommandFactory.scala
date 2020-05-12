package org.enso.languageserver.websocket.data.factory

import com.google.flatbuffers.FlatBufferBuilder
import org.enso.languageserver.protocol.data.filemanager.ReadFileCommand

object ReadFileCommandFactory {

  def create(
    path: Int
  )(implicit builder: FlatBufferBuilder): Int = {
    ReadFileCommand.startReadFileCommand(builder)
    ReadFileCommand.addPath(builder, path)
    ReadFileCommand.endReadFileCommand(builder)
  }

}
