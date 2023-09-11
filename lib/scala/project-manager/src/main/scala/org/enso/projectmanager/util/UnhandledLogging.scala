package org.enso.projectmanager.util

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import org.enso.logger.akka.AkkaConverter
import org.slf4j.event.Level

trait UnhandledLogging extends LazyLogging { this: Actor =>

  private val akkaLogLevel = AkkaConverter //LogLevel
    .fromString(context.system.settings.LogLevel)
    .orElse(Level.ERROR)

  override def unhandled(message: Any): Unit = {
    if (Level.WARN.toInt <= akkaLogLevel.toInt) {
      logger.warn("Received unknown message: {}", message.getClass)
    }
  }

}
