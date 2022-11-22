package org.enso.loggingservice.printers

import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.WSLogMessage

import java.io.PrintWriter
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.logging.{LogRecord, XMLFormatter}

/** Creates a new file in [[logPath]] and writes incoming log messages to
  * this file in XML format.
  *
  * @param logPath the file path to log
  */
class FileXmlPrinter(logPath: Path) extends Printer {

  private val writer    = initializeWriter()
  private val formatter = new XMLFormatter()

  /** @inheritdoc */
  override def print(message: WSLogMessage): Unit = {
    val lines = formatter.format(toLogRecord(message))
    writer.print(lines)
  }

  /** @inheritdoc */
  override def shutdown(): Unit = {
    writer.flush()
    writer.close()
  }

  /** Opens the log file for writing. */
  private def initializeWriter(): PrintWriter = {
    Option(logPath.getParent).foreach(Files.createDirectories(_))
    val writer = new PrintWriter(
      Files.newBufferedWriter(
        logPath,
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.WRITE
      )
    )
    writer.println(FileXmlPrinter.Header)
    writer
  }

  /** Converts [[WSLogMessage]] to java [[LogRecord]]. */
  private def toLogRecord(wsLogMessage: WSLogMessage): LogRecord = {
    val record =
      new LogRecord(LogLevel.toJava(wsLogMessage.level), wsLogMessage.message)
    record.setInstant(wsLogMessage.timestamp)
    record.setLoggerName(wsLogMessage.group)
    record
  }

}
object FileXmlPrinter {

  private val Header: String =
    "<?xml version='1.0' encoding='UTF-8'?><uigestures version='1.0'>"
}
