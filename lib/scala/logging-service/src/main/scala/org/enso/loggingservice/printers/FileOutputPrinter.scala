package org.enso.loggingservice.printers

import java.io.PrintWriter
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}

import org.enso.loggingservice.internal.DefaultLogMessageRenderer
import org.enso.loggingservice.internal.protocol.WSLogMessage

class FileOutputPrinter(logDirectory: Path) extends Printer {

  private val renderer = new DefaultLogMessageRenderer
  private val writer   = initializeWriter()

  def print(message: WSLogMessage): Unit = {
    val lines = renderer.render(message)
    writer.println(lines)
  }

  def shutdown(): Unit = {
    writer.flush()
    writer.close()
  }

  private def initializeWriter(): PrintWriter = {
    val logPath = logDirectory.resolve(makeLogFilename())
    Files.createDirectories(logDirectory)
    new PrintWriter(
      Files.newBufferedWriter(
        logPath,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE
      )
    )
  }

  private def makeLogFilename(): String = {
    val timestampZone = ZoneId.of("UTC")
    val timestamp = LocalDateTime
      .ofInstant(Instant.now(), timestampZone)
      .format(DateTimeFormatter.ofPattern("YYYYMMdd-HHmmss-SSS"))
    s"$timestamp-enso.log"
  }
}

object FileOutputPrinter {
  def create(logDirectory: Path): FileOutputPrinter =
    new FileOutputPrinter(logDirectory)
}
