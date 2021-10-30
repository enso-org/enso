package org.enso.runtimeversionmanager.runner

import org.enso.runtimeversionmanager.components.GraalRuntime

/** Represents a way of launching the JVM.
  *
  * @param executableName name of the `java` executable to run
  * @param javaHomeOverride if set, asks to override the JAVA_HOME environment
  *                         variable when launching the JVM
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
