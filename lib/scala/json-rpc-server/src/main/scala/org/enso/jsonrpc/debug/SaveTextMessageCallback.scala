package org.enso.jsonrpc.debug

import org.enso.jsonrpc.MessageHandler.WebMessage

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}

final class SaveTextMessageCallback private (path: Path)
    extends (WebMessage => Unit) {

  override def apply(webMessage: WebMessage): Unit = {
    val message = webMessage.message
    if (!SaveTextMessageCallback.isHeartbeat(message)) {
      Files.write(
        path,
        message.getBytes(StandardCharsets.UTF_8),
        StandardOpenOption.APPEND
      )
      Files.write(
        path,
        SaveTextMessageCallback.LINE_SEPARATOR_BYTES,
        StandardOpenOption.APPEND
      )
    }
  }
}

object SaveTextMessageCallback {

  private val MESSAGE_HEARTBEAT_HEAD = 54
  private val MESSAGE_HEARTBEAT_TEXT = """"method": "heartbeat/"""
  private val LINE_SEPARATOR_BYTES =
    System.lineSeparator().getBytes(StandardCharsets.UTF_8)

  /** Check if the message is a heartbeat.
    *
    * @param message the message to check
    * @return `true` if the message is a heartbeat message and `false` otherwise
    */
  private def isHeartbeat(message: String): Boolean =
    message.take(MESSAGE_HEARTBEAT_HEAD).contains(MESSAGE_HEARTBEAT_TEXT)

  def apply(path: Path): SaveTextMessageCallback = {
    Files.newBufferedWriter(path, StandardOpenOption.TRUNCATE_EXISTING).close()
    new SaveTextMessageCallback(path)
  }

  def apply(): SaveTextMessageCallback = {
    val tmpfile = Files.createTempFile("enso-messages-", ".txt")
    new SaveTextMessageCallback(tmpfile)
  }
}
