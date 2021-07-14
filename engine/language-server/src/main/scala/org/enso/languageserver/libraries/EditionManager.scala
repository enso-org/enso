package org.enso.languageserver.libraries

import akka.actor.Actor
import org.enso.distribution.DistributionManager

import java.nio.file.Path

class EditionManager(
  projectRoot: Path,
  distributionManager: DistributionManager
) extends Actor {
  override def receive: Receive = ???
}
