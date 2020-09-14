package org.enso.launcher.locking

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.components.RuntimeVersion

sealed trait Resource {
  def name:        String
  def waitMessage: String
}
object Resource {
  case object LauncherExecutable extends Resource {
    override def name: String = "launcher-executable"
    override def waitMessage: String =
      "Another process is modifying the launcher, " +
      "the current process must wait until that one completes its action."
  }
  case class Engine(version: SemVer) extends Resource {
    override def name: String = s"engine-$version"
    override def waitMessage: String =
      s"Another process is modifying engine $version, " +
      "the current process must wait until that one completes its action."
  }
  case class Runtime(version: RuntimeVersion) extends Resource {
    override def name: String = s"runtime-${version.graal}-${version.java}"
    override def waitMessage: String =
      s"Another process is modifying $version, " +
      "the current process must wait until that one completes its action."
  }
}
