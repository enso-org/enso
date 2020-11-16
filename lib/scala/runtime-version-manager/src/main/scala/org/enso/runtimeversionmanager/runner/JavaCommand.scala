package org.enso.runtimeversionmanager.runner

import org.enso.runtimeversionmanager.components.GraalRuntime

/** Represents a way of launching the JVM.
  *
  * Stores the name of the `java` executable to run and a possible JAVA_HOME
  * environment variable override.
  */
case class JavaCommand(
  executableName: String,
  javaHomeOverride: Option[String]
)

object JavaCommand {

  /** The [[JavaCommand]] representing the system-configured JVM.
    */
  def systemJavaCommand: JavaCommand = JavaCommand("java", None)

  /** The [[JavaCommand]] representing a managed [[GraalRuntime]].
    */
  def forRuntime(runtime: GraalRuntime): JavaCommand =
    JavaCommand(
      executableName = runtime.javaExecutable.toAbsolutePath.normalize.toString,
      javaHomeOverride =
        Some(runtime.javaHome.toAbsolutePath.normalize.toString)
    )

}
