package org.enso.languageserver.libraries

import akka.actor.Actor
import org.enso.distribution.DistributionManager

import scala.annotation.unused

class LocalLibraryManager(@unused distributionManager: DistributionManager)
    extends Actor {
  override def receive: Receive = ???
}
