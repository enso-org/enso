package org.enso.loggingservice

import org.enso.loggingservice.internal.server.Server
import org.enso.loggingservice.internal.serviceconnection.{Fallback, Service}
import org.enso.loggingservice.internal.{
  BlockingConsumerMessageQueue,
  InternalLogMessage,
  LoggerConnection
}
import org.enso.loggingservice.printers.StderrPrinter

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

  /**
    * Tries to set up the logging service, falling back to a simple logger if it
    * failed.
    *
    * The returned future will contain `true` if the original backend was set-up
    * or `false` if it had to fall back to stderr.
    */
  def setupWithFallbackToLocal(
    mode: WSLoggerMode,
    logLevel: LogLevel
  ): Future[Boolean] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    setup(mode, logLevel).map(_ => true).recoverWith { error =>
      System.err.println(s"Failed to initialize the logging server: $error.")
      System.err.println("Falling back to a simple stderr backend.")
      setup(WSLoggerMode.Local(), logLevel).map(_ => false)
    }
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

      val service = mode match {
        case WSLoggerMode.Client(_) => ???
        case WSLoggerMode.Server(port, interface, printers) =>
          Server.setup(interface, port, messageQueue, printers, logLevel)
        case WSLoggerMode.Local(printers) =>
          Fallback.setup(printers, logLevel, messageQueue)
      }
      currentService = Some(service)
    }
  }
}
