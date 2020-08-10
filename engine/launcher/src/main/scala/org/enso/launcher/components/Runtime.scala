package org.enso.launcher.components

import java.nio.file.{Files, Path}

import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.OS

import scala.util.Try

/**
  * Represents a runtime component.
  *
  * @param version version of the component
  * @param path path to the component
  */
case class Runtime(version: RuntimeVersion, path: Path) {

  /**
    * @inheritdoc
    */
  override def toString: String =
    s"GraalVM ${version.graal}-java${version.java}"

  def javaHome: Path =
    OS.operatingSystem match {
      case OS.Linux   => path
      case OS.MacOS   => path / "Contents" / "Home"
      case OS.Windows => path
    }

  def javaExecutable: Path = {
    val executableName = if (OS.isWindows) "java.exe" else "java"
    javaHome / "bin" / executableName
  }

  def isValid: Boolean =
    Files.exists(javaExecutable) && Files.isExecutable(javaExecutable)
}
