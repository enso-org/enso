package org.enso.languageserver.libraries.handlers

import akka.actor.{Actor, ActorRef}
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.util.UnhandledLogging

import scala.concurrent.duration.FiniteDuration

class LibraryGetMetadataHandler(
  requestTimeout: FiniteDuration,
  libraryManager: LibraryManager
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = ???
}
