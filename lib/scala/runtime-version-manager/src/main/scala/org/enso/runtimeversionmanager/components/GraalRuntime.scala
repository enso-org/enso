package org.enso.runtimeversionmanager.components

import java.nio.file.{Files, Path}

import org.enso.runtimeversionmanager.OS
import org.enso.runtimeversionmanager.FileSystem.PathSyntax

import scala.util.{Failure, Success, Try}

/** Represents a runtime component.
  *
  * @param version version of the component
  * @param path path to the component
  */
case class GraalRuntime(version: GraalVMVersion, path: Path) {

  /** @inheritdoc
    */
  override def toString: String =
    s"GraalVM ${version.graalVersion}-java${version.java}"

  /** The path to the JAVA_HOME directory associated with this runtime.
    */
  def javaHome: Path =
    OS.operatingSystem match {
      case OS.Linux   => path
      case OS.MacOS   => path / "Contents" / "Home"
      case OS.Windows => path
    }

  /** The path to the `java` executable associated with this runtime.
    */
  def javaExecutable: Path = {
    val executableName = if (OS.isWindows) "java.exe" else "java"
    javaHome / "bin" / executableName
  }

  /** Checks if the installation is not corrupted and reports any issues as
    * failures.
    */
  def ensureValid(): Try[Unit] =
    if (!Files.exists(javaExecutable))
      Failure(
        CorruptedComponentError(
          s"Runtime's java executable (expected at " +
          s"`${javaExecutable.toAbsolutePath.normalize}`) is missing."
        )
      )
    else if (!Files.isExecutable(javaExecutable))
      Failure(
        CorruptedComponentError(
          "Runtime's java executable is not marked as executable."
        )
      )
    else Success(())
}
