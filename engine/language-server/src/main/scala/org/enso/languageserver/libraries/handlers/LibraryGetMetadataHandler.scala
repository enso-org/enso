package org.enso.languageserver.libraries.handlers

import akka.actor.Actor
import com.typesafe.scalalogging.LazyLogging
import org.enso.languageserver.util.UnhandledLogging

import scala.annotation.unused
import scala.concurrent.duration.FiniteDuration

class LibraryGetMetadataHandler(
  @unused requestTimeout: FiniteDuration,
  @unused libraryManager: Any
) extends Actor
    with LazyLogging
    with UnhandledLogging {
  override def receive: Receive = ???
}
