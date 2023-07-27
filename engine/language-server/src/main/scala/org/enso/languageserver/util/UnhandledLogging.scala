package org.enso.languageserver.util

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import org.enso.logger.akka.AkkaConverter
//import org.enso.loggingservice.LogLevel
import org.slf4j.event.Level

trait UnhandledLogging extends LazyLogging { this: Actor =>

  private val akkaLogLevel = AkkaConverter
    .fromString(context.system.settings.LogLevel)
    .orElse(Level.ERROR)
  //.getOrElse(LogLevel.Error)

  override def unhandled(message: Any): Unit = {
    if (Level.WARN.toInt <= akkaLogLevel.toInt) {
      //if (implicitly[Ordering[]].lteq(LogLevel.Warning, akkaLogLevel)) {
      logger.warn("Received unknown message [{}].", message.getClass)
    }
  }
}
