package org.enso.runtimeversionmanager.runner

import org.enso.runtimeversionmanager.components.Engine

/** Executable command used to trigger a Runner. Can be either a Java command or a native image executable.
  */
trait ExecCommand {
  // Path to executable
  def path:                                                   String
  def cmdArguments(engine: Engine, jvmSettings: JVMSettings): Seq[String]
  def javaHome:                                               Option[String]
}
