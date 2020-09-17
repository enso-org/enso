package org.enso.launcher.locking

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.components.RuntimeVersion

trait Resource {
  def name:        String
  def waitMessage: String
}
object Resource {
  case object LauncherExecutable extends Resource {
    override def name: String = "launcher-executable"
    override def waitMessage: String =
      "Another upgrade is in progress, " +
      "the current process must wait until it is completed."
  }

  /**
    * This resource is held when adding or removing any components.
    */
  case object AddOrRemoveComponents extends Resource {
    override def name: String = "add-remove-components"
    override def waitMessage: String =
      "Another process is adding or removing components, " +
      "the current process must wait until it finishes."
  }
  case class Engine(version: SemVer) extends Resource {
    override def name: String = s"engine-$version"
    override def waitMessage: String =
      s"Another process is using engine $version, " +
      "the current process must wait until other processes complete."
  }
  case class Runtime(version: RuntimeVersion) extends Resource {
    override def name: String = s"runtime-${version.graal}-${version.java}"
    override def waitMessage: String =
      s"Another process is using $version, " +
      "the current process must wait until other processes complete."
  }
}
