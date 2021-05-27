package org.enso.projectmanager.util

import akka.actor.Actor
import akka.event.Logging

trait UnhandledLogging { this: Actor =>

  private val akkaLogger = Logging(context.system, this)

  override def unhandled(message: Any): Unit =
    akkaLogger.warning("Received unknown message: {}", message)

}
