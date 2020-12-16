package org.enso.loggingservice.internal
import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.protocol.WSLogMessage
import org.enso.loggingservice.printers.StderrPrinter

/** A message queue for use in testing.
  *
  * It has a smaller buffer and ignores messages from a certain log level.
  *
  * @param logLevel specifies which messages will be printed to stderr if no
  *                 service is set-up
  * @param isLoggingServiceSetUp a function used to check if a logging service
  *                              is set up
  */
class TestMessageQueue(logLevel: LogLevel, isLoggingServiceSetUp: () => Boolean)
    extends BlockingConsumerMessageQueue(bufferSize = 100) {

  private def shouldKeepMessage(
    message: Either[InternalLogMessage, WSLogMessage]
  ): Boolean = message match {
    case Left(value)  => logLevel.shouldLog(value.level)
    case Right(value) => logLevel.shouldLog(value.level)
  }

  private val overridePrinter = StderrPrinter.create()

  /** @inheritdoc */
  override def send(message: Either[InternalLogMessage, WSLogMessage]): Unit =
    if (isLoggingServiceSetUp()) {
      if (shouldKeepMessage(message))
        overridePrinter.print(message.fold(_.toLogMessage, identity))
    } else {
      super.send(message)
    }
}
