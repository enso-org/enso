package org.enso.languageserver.util

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import org.enso.loggingservice.LogLevel

trait UnhandledLogging extends LazyLogging { this: Actor =>

  private val akkaLogLevel = LogLevel
    .fromString(context.system.settings.LogLevel)
    .getOrElse(LogLevel.Error)

  override def unhandled(message: Any): Unit = {
    if (implicitly[Ordering[LogLevel]].lteq(LogLevel.Warning, akkaLogLevel)) {
      logger.warn("Received unknown message: {}", message.getClass)
    }
  }
}
