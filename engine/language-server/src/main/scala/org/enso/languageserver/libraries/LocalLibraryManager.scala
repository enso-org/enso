package org.enso.languageserver.libraries

import akka.actor.Actor
import org.enso.distribution.DistributionManager
import org.enso.languageserver.libraries.LocalLibraryManagerProtocol._

import scala.annotation.unused

class LocalLibraryManager(@unused distributionManager: DistributionManager)
    extends Actor {
  override def receive: Receive = { case request: Request =>
    request match {
      case GetMetadata(libraryName)                                 =>
      case SetMetadata(libraryName, description, tagLine)           =>
      case ListLocalLibraries                                       =>
      case Publish(libraryName, authToken, bumpVersionAfterPublish) =>
    }
  }
}
