package org.enso.loggingservice

import org.enso.loggingservice.internal.service.{Client, Local, Server, Service}
import org.enso.loggingservice.internal.{
  BlockingConsumerMessageQueue,
  InternalLogMessage,
  InternalLogger,
  LoggerConnection
}
import org.enso.loggingservice.printers.StderrPrinter

import scala.concurrent.Future

/**
  * Manages the logging service.
  */
object LoggingServiceManager {
  private val messageQueue           = new BlockingConsumerMessageQueue()
  private var currentLevel: LogLevel = LogLevel.Trace

  /**
    * The default [[LoggerConnection]] that should be used by all backends which
    * want to use the logging service.
    */
  object Connection extends LoggerConnection {

    /**
      * @inheritdoc
      */
    override def send(message: InternalLogMessage): Unit =
      messageQueue.send(Left(message))

    /**
      * @inheritdoc
      */
    override def logLevel: LogLevel = currentLevel
  }

  private var currentService: Option[Service] = None

  /**
    * Sets up the logging service, but in a separate thread to avoid stalling
    * the application.
    *
    * The returned [[InitializationResult]] depends on the mode.
    */
  def setup[InitializationResult](
    mode: LoggerMode[InitializationResult],
    logLevel: LogLevel
  ): Future[InitializationResult] = {
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
  def setupWithFallback(
    mode: LoggerMode[_],
    fallbackMode: LoggerMode[_],
    logLevel: LogLevel
  ): Future[Boolean] = {
    import scala.concurrent.ExecutionContext.Implicits.global
    setup(mode, logLevel).map(_ => true).recoverWith { error =>
      InternalLogger.error(s"Failed to initialize the logging server: $error")
      InternalLogger.error("Falling back to a simple stderr backend.")
      setup(fallbackMode, logLevel).map(_ => false)
    }
  }

  /**
    * Shuts down the logging service if it was initialized or runs
    * [[handleMissingLogger]] to handle logs that would be dropped due to the
    * logging service never being initialized.
    *
    * This method is also called as a shutdown hook, but it is good to call it
    * before shutting down to ensure that everything has a chance to terminate
    * correctly before the application exits. It can be safely called multiple
    * times.
    */
  def tearDown(): Unit = {
    val service = currentService.synchronized {
      val service = currentService
      currentService = None
      service
    }

    service match {
      case Some(running) => running.terminate()
      case None          => handleMissingLogger()
    }
  }

  Runtime.getRuntime.addShutdownHook(new Thread(() => tearDown()))

  /**
    * Terminates the currently running logging service (if any) and replaces it
    * with a fallback logging service.
    *
    * Can be used if the currently logging service fails after initialization
    * and has to be shutdown.
    */
  def replaceWithFallback(): Unit = {
    val fallback =
      Local.setup(currentLevel, messageQueue, Seq(StderrPrinter.create()))
    val service = currentService.synchronized {
      val service = currentService
      currentService = Some(fallback)
      service
    }

    service.foreach(_.terminate())
  }

  /**
    * Removes any pending logs (so that [[handleMissingLogger]] will not print
    * them).
    *
    * An internal method that is only used by [[TestLogger]].
    */
  def dropPendingLogs(): Unit = messageQueue.drain(LogLevel.Off)

  /**
    * Prints any messages that have been buffered but have not been logged yet
    * due to no loggers being active.
    */
  private def handleMissingLogger(): Unit = {
    val danglingMessages = messageQueue.drain(currentLevel)
    if (danglingMessages.nonEmpty) {
      InternalLogger.error(
        "It seems that the logging service was never set up, " +
        "or log messages were reported after it has been terminated. " +
        "These messages are printed below:"
      )
      val stderrPrinter = StderrPrinter.create()
      danglingMessages.foreach { message =>
        stderrPrinter.print(message)
      }
    }
  }

  private def doSetup[InitializationResult](
    mode: LoggerMode[InitializationResult],
    logLevel: LogLevel
  ): InitializationResult = {
    currentService.synchronized {
      if (currentService.isDefined) {
        throw new IllegalStateException(
          "The logging service has already been set up."
        )
      }

      val (service, result): (Service, InitializationResult) = mode match {
        case LoggerMode.Client(endpoint) =>
          (Client.setup(endpoint, messageQueue, logLevel), ())
        case LoggerMode.Server(printers, port, interface) =>
          val server = Server.setup(
            interface,
            port.getOrElse(0),
            messageQueue,
            printers,
            logLevel
          )
          (server, server.getBinding())
        case LoggerMode.Local(printers) =>
          (Local.setup(logLevel, messageQueue, printers), ())
      }
      currentService = Some(service)
      result
    }
  }
}
