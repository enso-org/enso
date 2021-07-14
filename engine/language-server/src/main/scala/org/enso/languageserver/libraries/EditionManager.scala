package org.enso.languageserver.libraries

import akka.actor.Actor
import org.enso.distribution.DistributionManager

import java.nio.file.Path
import scala.annotation.unused

class EditionManager(
  @unused projectRoot: Path,
  @unused distributionManager: DistributionManager
) extends Actor {
  override def receive: Receive = ???
}
