package org.enso.loggingservice.printers

import java.io.PrintWriter
import java.nio.file.{Files, Path, StandardOpenOption}
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}

import org.enso.loggingservice.internal.DefaultLogMessageRenderer
import org.enso.loggingservice.internal.protocol.WSLogMessage

/** Creates a new file in [[logDirectory]] and writes incoming log messages to
  * this file.
  *
  * @param logDirectory the directory to create the logfile in
  * @param suffix a suffix to be added to the filename
  * @param printExceptions whether to print exceptions attached to the log
  *                        messages
  */
class FileOutputPrinter(
  logDirectory: Path,
  suffix: String,
  printExceptions: Boolean
) extends Printer {

  private val renderer = new DefaultLogMessageRenderer(printExceptions)
  private val writer   = initializeWriter()

  /** @inheritdoc */
  override def print(message: WSLogMessage): Unit = {
    val lines = renderer.render(message)
    writer.println(lines)
    // TODO [RW] we may consider making flushing configurable as it is mostly
    //  useful for debugging crashes, whereas for usual usecases buffering could
    //  give slightly better performance
    writer.flush()
  }

  /** @inheritdoc */
  override def shutdown(): Unit = {
    writer.flush()
    writer.close()
  }

  /** Opens the log file for writing. */
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

  /** Creates a log filename that is created based on the current timestamp. */
  private def makeLogFilename(): String = {
    val timestampZone = ZoneId.of("UTC")
    val timestamp = LocalDateTime
      .ofInstant(Instant.now(), timestampZone)
      .format(DateTimeFormatter.ofPattern("YYYYMMdd-HHmmss-SSS"))
    s"$timestamp-$suffix.log"
  }
}

object FileOutputPrinter {

  /** Creates a new [[FileOutputPrinter]]. */
  def create(
    logDirectory: Path,
    suffix: String,
    printExceptions: Boolean
  ): FileOutputPrinter =
    new FileOutputPrinter(logDirectory, suffix, printExceptions)
}
