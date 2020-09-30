package org.enso.runner

import akka.http.scaladsl.model.Uri
import com.typesafe.scalalogging.Logger
import org.enso.loggingservice.printers.StderrPrinter
import org.enso.loggingservice.{LogLevel, WSLoggerManager, WSLoggerMode}

import scala.util.{Failure, Success}

object RunnerLogging {
  def setup(connectionUri: Option[Uri], logLevel: LogLevel): Unit = {
    val local =
      WSLoggerMode.Local(Seq(StderrPrinter.create(printStackTraces = true)))
    connectionUri match {
      case Some(uri) =>
        import scala.concurrent.ExecutionContext.Implicits.global
        WSLoggerManager
          .setupWithFallback(
            WSLoggerMode.Client(uri),
            local,
            logLevel
          )
          .onComplete {
            case Failure(exception) =>
              System.err.println(s"Failed to initialize logging: $exception")
            case Success(connected) =>
              val logger = Logger[RunnerLogging.type]
              if (connected) {
                logger.trace(s"Connected to logging service at `$uri`.")
              }
          }
      case None =>
        WSLoggerManager.setup(local, logLevel)
    }
  }

}
