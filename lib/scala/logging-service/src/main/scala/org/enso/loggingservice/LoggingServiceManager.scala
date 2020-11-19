package org.enso.loggingservice

import org.enso.loggingservice.internal.service.{Client, Local, Server, Service}
import org.enso.loggingservice.internal.{
  BlockingConsumerMessageQueue,
  InternalLogMessage,
  InternalLogger,
  LoggerConnection
}
import org.enso.loggingservice.printers.{Printer, StderrPrinter}

import scala.concurrent.{ExecutionContext, Future}

/** Manages the logging service.
  */
object LoggingServiceManager {
  private val messageQueue           = new BlockingConsumerMessageQueue()
  private var currentLevel: LogLevel = LogLevel.Trace

  /** The default [[LoggerConnection]] that should be used by all backends which
    * want to use the logging service.
    */
  object Connection extends LoggerConnection {

    /** @inheritdoc
      */
    override def send(message: InternalLogMessage): Unit =
      messageQueue.send(Left(message))

    /** @inheritdoc
      */
    override def logLevel: LogLevel = currentLevel
  }

  private var currentService: Option[Service] = None

  /** Sets up the logging service, but in a separate thread to avoid stalling
    * the application.
    *
    * The returned [[InitializationResult]] depends on the mode.
    *
    * It is important to note that any printers passed inside of `mode` are from
    * now on owned by the setup function and the created service, so if service
    * creation fails, they will be shutdown alongside service termination. Any
    * printers passed to this function must not be reused.
    *
    * @param mode [[LoggerMode]] to setup
    * @param logLevel specifies which log level should be used for logs from
    *                 this instance; this log level does not affect remote log
    *                 levels in server mode
    * @param executionContext execution context to run the initialization in
    * @return a future that will complete once the logger is initialized
    */
  def setup[InitializationResult](
    mode: LoggerMode[InitializationResult],
    logLevel: LogLevel
  )(implicit
    executionContext: ExecutionContext =
      scala.concurrent.ExecutionContext.Implicits.global
  ): Future[InitializationResult] = {
    currentLevel = logLevel
    Future(doSetup(mode, logLevel))
  }

  /** Shuts down the logging service if it was initialized or runs
    * [[handlePendingMessages]] to handle logs that would be dropped due to the
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
      case None          =>
    }

    handlePendingMessages()
  }

  Runtime.getRuntime.addShutdownHook(new Thread(() => tearDown()))

  /** Terminates the currently running logging service (if any) and replaces it
    * with a fallback logging service.
    *
    * Can be used if the currently logging service fails after initialization
    * and has to be shutdown.
    */
  def replaceWithFallback(
    printers: Seq[Printer] = Seq(StderrPrinter.create())
  ): Unit = {
    val fallback =
      Local.setup(currentLevel, messageQueue, printers)
    val previousService = currentService.synchronized {
      val previous = currentService
      currentService = Some(fallback)
      previous
    }

    previousService match {
      case Some(service) =>
        service.terminate()
      case None =>
    }
  }

  /** Removes any pending logs (so that [[handlePendingMessages]] will not print
    * them).
    *
    * An internal method that is only used by [[TestLogger]].
    */
  def dropPendingLogs(): Unit = messageQueue.drain(LogLevel.Off)

  /** Prints any messages that have been buffered but have not been logged yet
    * due to no loggers being active.
    */
  private def handlePendingMessages(): Unit = {
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
