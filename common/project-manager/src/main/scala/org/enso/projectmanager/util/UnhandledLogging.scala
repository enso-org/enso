package org.enso.projectmanager.util

import akka.actor.{Actor, ActorLogging}

trait UnhandledLogging { this: Actor with ActorLogging =>

  override def unhandled(message: Any): Unit =
    log.warning("Received unknown message: {}", message)

}
