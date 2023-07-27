package org.enso.runner

import akka.http.scaladsl.model.Uri
import com.typesafe.scalalogging.Logger
import org.enso.logger.masking.Masking

import scala.annotation.unused
//import org.enso.loggingservice.printers.StderrPrinter
import org.slf4j.event.Level
//import org.enso.loggingservice.{LogLevel, LoggerMode, LoggingServiceManager}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/** Manages setting up the logging service within the runner.
  */
object RunnerLogging {

  /** Sets up the runner's logging service.
    *
    * If `connectionUri` is provided it tries to connect to a logging service
    * server and pass logs to it. If it is not provided, or the connection could
    * not be established, falls back to logging to standard error output.
    *
    * @param connectionUri optional uri of logging service server to connect to
    * @param logLevel log level to use for the runner and runtime
    * @param logMasking switches log masking on and off
    */
  def setup(
    connectionUri: Option[Uri],
    logLevel: Level,
    logMasking: Boolean
  ): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    Masking.setup(logMasking)
    val loggerSetup = connectionUri match {
      case Some(uri) =>
        // TODO
        /*LoggingServiceManager
          .setup(
            LoggerMode.Client(uri),
            logLevel
          )*/
        Future
          .successful(())
          .map { _ =>
            logger.trace("Connected to logging service at [{}].", uri)
          }
          .recoverWith { _ =>
            logger.error(
              "Failed to connect to the logging service server, " +
              "falling back to local logging."
            )
            setupLocalLogger(logLevel)
          }
      case None =>
        setupLocalLogger(logLevel)
    }
    loggerSetup.onComplete {
      case Failure(exception) =>
        System.err.println(s"Failed to initialize logging: $exception")
        exception.printStackTrace()
      case Success(_) =>
    }
  }

  private def setupLocalLogger(@unused logLevel: Level): Future[Unit] = {
    // TODO
    Future.successful(())
  }

  /*LoggingServiceManager
      .setup(
        LoggerMode.Local(
          Seq(StderrPrinter.create(printExceptions = true))
        ),
        logLevel
      )*/

  /*private def setupLocalLogger(logLevel: LogLevel): Future[Unit] =
    LoggingServiceManager
      .setup(
        LoggerMode.Local(
          Seq(StderrPrinter.create(printExceptions = true))
        ),
        logLevel
      )*/

  private val logger = Logger[RunnerLogging.type]

  /** Shuts down the logging service gracefully.
    */
  def tearDown(): Unit =
    // TODO
    ()
  //LoggingServiceManager.tearDown()
}
