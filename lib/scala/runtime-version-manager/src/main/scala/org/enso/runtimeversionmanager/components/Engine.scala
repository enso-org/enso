package org.enso.runtimeversionmanager.components

import java.nio.file.{Files, Path}

import nl.gn0s1s.bump.SemVer
import org.enso.logger.masking.MaskedPath
import org.enso.distribution.FileSystem.PathSyntax
import org.enso.runtimeversionmanager.components.Manifest.JVMOption

import scala.util.{Failure, Success, Try}

/** Represents an engine component.
  *
  * @param version version of the component
  * @param path path to the component
  * @param manifest manifest of the engine release
  */
case class Engine(version: SemVer, path: Path, manifest: Manifest) {

  /** @inheritdoc
    */
  override def toString: String = {
    val broken = if (isMarkedBroken) " (marked as broken)" else ""
    s"Enso Engine $version$broken"
  }

  /** The runtime version that is associated with this engine and should be used
    * for running it.
    */
  def graalRuntimeVersion: GraalVMVersion = manifest.runtimeVersion

  /** The GraalVM distribution policy changed a lot since GraalVM 23.0.0 for JDK 21.
    * The newest GraalVM is now distributed as artifacts from the Maven central, and therefore,
    * does not need to be downloaded at runtime.
    *
    * @see https://medium.com/graalvm/truffle-unchained-13887b77b62c
    * @return true if a separate GraalVM distribution download is needed.
    */
  def needsGraalDistribution: Boolean = {
    !graalRuntimeVersion.isUnchained
  }

  /** A set of JVM options that should be added when running this engine.
    */
  def defaultJVMOptions: Seq[JVMOption] = manifest.jvmOptions

  /** Path to the directory containing all the explicit JPMS modules as Jar archives.
    * @return
    */
  def componentDirPath: Path = path / "component"

  /** Path to the runner JAR.
    */
  def runnerPath: Path = {
    if (needsGraalDistribution) {
      componentDirPath / "runner.jar"
    } else {
      componentDirPath / "runner" / "runner.jar"
    }
  }

  /** Path to the runtime JAR.
    */
  def runtimePath: Path = componentDirPath / "runtime.jar"

  /** Checks if the installation is not corrupted and reports any issues as
    * failures.
    */
  def ensureValid(): Try[Unit] =
    if (!Files.exists(runnerPath))
      Failure(
        CorruptedComponentError(
          s"Engine's runner.jar (expected at " +
          s"`${MaskedPath(runnerPath).applyMasking()}`) is missing."
        )
      )
    else if (!Files.exists(runtimePath))
      Failure(
        CorruptedComponentError(
          s"`Engine's runtime.jar (expected at " +
          s"${MaskedPath(runtimePath).applyMasking()}`) is missing."
        )
      )
    else Success(())

  /** Returns if the engine release was marked as broken when it was being
    * installed.
    *
    * Releases marked as broken are not considered when looking for the latest
    * installed release and issue a warning when they are used.
    */
  def isMarkedBroken: Boolean = manifest.brokenMark

  /** Specifies if the engine version comes from a nightly release.
    *
    * See `docs/distribution/nightly.md` for more information.
    */
  def isNightly: Boolean = Engine.isNightly(version)
}

object Engine {

  /** The infix that should be included in the pre-release part of the semantic
    * versioning string that describes the engine version to indicate that this
    * is a prerelease.
    */
  def nightlyInfix = "SNAPSHOT"

  /** Specifies if the engine version comes from a nightly release.
    *
    * See `docs/distribution/nightly.md` for more information.
    */
  def isNightly(version: SemVer): Boolean =
    version.preRelease.exists(_.contains(nightlyInfix))
}
