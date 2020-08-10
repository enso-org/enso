package org.enso.launcher.components

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.launcher.FileSystem.PathSyntax

/**
  * Represents an engine component.
  *
  * @param version version of the component
  * @param path path to the component
  * @param manifest manifest of the engine release
  */
case class Engine(version: SemVer, path: Path, manifest: Manifest) {

  /**
    * @inheritdoc
    */
  override def toString: String =
    s"Enso Engine $version"

  def graalRuntimeVersion: RuntimeVersion      = manifest.runtimeVersion
  def defaultJVMOptions: Seq[(String, String)] = manifest.jvmOptions

  def runnerPath: Path  = path / "component" / "runner.jar"
  def runtimePath: Path = path / "component" / "runtime.jar"

  def isValid: Boolean =
    Files.exists(runnerPath) && Files.exists(runtimePath)
}
