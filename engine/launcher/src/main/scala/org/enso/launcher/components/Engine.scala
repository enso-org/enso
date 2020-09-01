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
  override def toString: String = {
    val broken = if (isMarkedBroken) " (marked as broken)" else ""
    s"Enso Engine $version$broken"
  }

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

  /**
    * Returns if the engine release was marked as broken when it was being
    * installed.
    *
    * Releases marked as broken are not considered when looking for the latest
    * installed release and issue a warning when they are used.
    */
  def isMarkedBroken: Boolean = manifest.brokenMark
}
