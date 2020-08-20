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

  /**
    * The runtime version that is associated with this engine and should be used
    * for running it.
    */
  def graalRuntimeVersion: RuntimeVersion = manifest.runtimeVersion

  /**
    * A set of JVM options that should be added when running this engine.
    */
  def defaultJVMOptions: Seq[Manifest.JVMOption] = manifest.jvmOptions

  /**
    * Path to the runner JAR.
    */
  def runnerPath: Path = path / "component" / "runner.jar"

  /**
    * Path to the runtime JAR.
    */
  def runtimePath: Path = path / "component" / "runtime.jar"

  /**
    * Returns if the installation seems not-corrupted.
    */
  def isValid: Boolean =
    Files.exists(runnerPath) && Files.exists(runtimePath)
}
