package org.enso.jsonrpc.debug

import org.enso.jsonrpc.MessageHandler.WebMessage
import org.enso.jsonrpc.debug.SaveTextMessageCallback.{
  LINE_SEPARATOR_BYTES,
  MESSAGE_HEARTBEAT_HEAD,
  MESSAGE_HEARTBEAT_TEXT
}

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

final class SaveTextMessageCallback private (path: Path)
    extends (WebMessage => Unit) {

  override def apply(webMessage: WebMessage): Unit = {
    val message = webMessage.message
    if (
      !message.take(MESSAGE_HEARTBEAT_HEAD).contains(MESSAGE_HEARTBEAT_TEXT)
    ) {
      Files.write(
        path,
        message.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.APPEND
      )
      Files.write(path, LINE_SEPARATOR_BYTES, StandardOpenOption.APPEND)
    }
  }
}

object SaveTextMessageCallback {

  private val MESSAGE_HEARTBEAT_HEAD = 54
  private val MESSAGE_HEARTBEAT_TEXT = """"method": "heartbeat/"""
  private val LINE_SEPARATOR_BYTES =
    System.lineSeparator().getBytes(StandardCharsets.UTF_8)

  def apply(path: Path): SaveTextMessageCallback = {
    Files.newBufferedWriter(path, StandardOpenOption.TRUNCATE_EXISTING).close()
    new SaveTextMessageCallback(path)
  }

  def apply(): SaveTextMessageCallback = {
    val tmpfile = Files.createTempFile("enso-messages-", ".log")
    new SaveTextMessageCallback(tmpfile)
  }
}
