package org.enso.jsonrpc.debug

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.Base64

final class SaveBinaryMessageCallback private (path: Path)
    extends (ByteBuffer => Unit) {

  override def apply(message: ByteBuffer): Unit = {
    val bytes = Array.ofDim[Byte](message.remaining())
    message.get(bytes)
    val base64 = Base64.getEncoder.encode(bytes)
    Files.write(path, base64, StandardOpenOption.APPEND)
    Files.write(
      path,
      SaveBinaryMessageCallback.LINE_SEPARATOR_BYTES,
      StandardOpenOption.APPEND
    )
  }

}

object SaveBinaryMessageCallback {

  private val LINE_SEPARATOR_BYTES =
    System.lineSeparator().getBytes(StandardCharsets.UTF_8)

  def apply(path: Path): SaveBinaryMessageCallback = {
    Files
      .newBufferedWriter(
        path,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )
      .close()
    new SaveBinaryMessageCallback(path)
  }
}
