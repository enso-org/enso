package org.enso.jsonrpc.debug

import akka.actor.{Actor, Props}
import org.enso.jsonrpc.MessageHandler
import org.enso.jsonrpc.debug.MessageWriter.TimestampSeparator

import java.io.BufferedWriter
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.time.Clock
import java.util.Base64

/** Writes messages to the provided destination.
  *
  * @param clock the system clock
  * @param writer the destination to write messages
  */
final class MessageWriter(clock: Clock, writer: BufferedWriter) extends Actor {

  override def postStop(): Unit = {
    writer.close()
  }

  override def receive: Receive = {
    case message: MessageHandler.WebMessage =>
      writeTextEntry(message.message)

    case message: ByteBuffer =>
      writeBinaryEntry(message)
  }

  private def writeTextEntry(message: String): Unit = {
    writer.write(timestamp)
    writer.write(TimestampSeparator)
    writer.write(message)
  }

  private def writeBinaryEntry(message: ByteBuffer): Unit = {
    val bytes  = messageBytes(message)
    val base64 = Base64.getEncoder.encodeToString(bytes)
    writeTextEntry(base64)
    writer.newLine()
  }

  private def timestamp: String =
    clock.instant().toString

  private def messageBytes(message: ByteBuffer): Array[Byte] = {
    val bytes = Array.ofDim[Byte](message.remaining())
    message.get(bytes)
    bytes
  }
}

object MessageWriter {

  private val TimestampSeparator = " "

  def props(path: Path): Props = {
    val clock = Clock.systemUTC()
    val writer: BufferedWriter =
      Files.newBufferedWriter(path, StandardCharsets.UTF_8)
    Props(new MessageWriter(clock, writer))
  }
}
