package org.enso.loggingservice

import org.enso.loggingservice.internal.serviceconnection.{Fallback, Service}
import org.enso.loggingservice.internal.{
  BlockingConsumerMessageQueue,
  InternalLogMessage,
  LoggerConnection
}
import org.enso.loggingservice.printers.{Printer, StderrPrinter}

import scala.concurrent.Future

object WSLoggerManager {
  private val messageQueue           = new BlockingConsumerMessageQueue()
  private var currentLevel: LogLevel = LogLevel.Trace
  object Connection extends LoggerConnection {
    override def send(message: InternalLogMessage): Unit =
      messageQueue.send(Left(message))
    override def logLevel: LogLevel = currentLevel
  }

  private var currentService: Option[Service] = None

  /**
    * Sets up the logging service, but in a separate thread to avoid stalling
    * the application.
    */
  def setup(mode: WSLoggerMode, logLevel: LogLevel): Future[Unit] = {
    currentLevel = logLevel
    import scala.concurrent.ExecutionContext.Implicits.global
    Future(doSetup(mode, logLevel))
  }

  def tearDown(): Unit = {
    val service = currentService.synchronized { currentService }
    service match {
      case Some(running) => running.terminate()
      case None          => handleMissingLogger()
    }
  }

  Runtime.getRuntime.addShutdownHook(new Thread(() => tearDown()))

  private def handleMissingLogger(): Unit = {
    val danglingMessages = messageQueue.drain()
    if (danglingMessages.nonEmpty) {
      System.err.println(
        "It seems that the logging service was never set up, " +
        "or log messages were reported after it has been terminated. " +
        "These messages are printed below:"
      )
      danglingMessages.foreach { message =>
        if (currentLevel.shouldLog(message.logLevel)) {
          StderrPrinter.print(message)
        }
      }
    }
  }

  private def doSetup(mode: WSLoggerMode, logLevel: LogLevel): Unit = {
    currentService.synchronized {
      if (currentService.isDefined) {
        throw new IllegalStateException(
          "The logging service has already been set up."
        )
      }
      mode match {
        case WSLoggerMode.Client(_)       =>
        case WSLoggerMode.Server(_, _, _) =>
        case WSLoggerMode.Local(config)   => setUpFallback(config, logLevel)
      }
    }
  }

  private def setUpFallback(
    printers: Seq[Printer],
    logLevel: LogLevel
  ): Unit = {
    currentService = Some(Fallback.setup(printers, logLevel, messageQueue))
  }
}
