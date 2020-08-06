package org.enso.launcher.components

import java.io.{FileReader, IOException}
import java.nio.file.Path

import io.circe.{yaml, Decoder, DecodingFailure}
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.Logger

/**
  * Version information identifying the runtime that can be used with an engine
  * release.
  *
  * @param graal version of the GraalVM
  * @param java Java version of the GraalVM flavour that should be used
  */
case class RuntimeVersion(graal: SemVer, java: String)

/**
  * Contains release metadata read from the manifest file that is attached to
  * each release.
  *
  * @param minimumLauncherVersion The minimum required version of the launcher
  *                               that can be used to run this engine release.
  *                               Earlier launcher versions should not be able
  *                               to download this release, but print a message
  *                               that the launcher needs an upgrade.
  * @param graalVMVersion the version of the GraalVM runtime that has to be
  *                       used with this engine
  * @param graalJavaVersion the java version of that GraalVM runtime
  */
case class Manifest(
  minimumLauncherVersion: SemVer,
  graalVMVersion: SemVer,
  graalJavaVersion: String
) {

  /**
    * Returns a [[RuntimeVersion]] which encapsulates all version information
    * needed to find the runtime required for this release.
    */
  def runtimeVersion: RuntimeVersion =
    RuntimeVersion(graalVMVersion, graalJavaVersion)
}

object Manifest {

  /**
    * Defines the name under which the manifest is included in the releases.
    */
  val DEFAULT_MANIFEST_NAME = "manifest.yaml"

  /**
    * Tries to load the manifest at the given path.
    *
    * Returns None if the manifest could not be opened or could not be parsed.
    */
  def load(path: Path): Option[Manifest] = {
    try {
      val reader = new FileReader(path.toFile)
      try {
        yaml.parser.parse(reader).flatMap(_.as[Manifest]) match {
          case Left(error) =>
            Logger.debug(s"Error loading the manifest file `$path`: $error")
            None
          case Right(value) => Some(value)
        }
      } finally {
        reader.close()
      }
    } catch {
      case e: IOException =>
        Logger.debug(s"Cannot load file `$path`: $e", e)
        None
    }

  }

  /**
    * Parses the manifest from a string containing a YAML definition.
    *
    * Returns None if the definition cannot be parsed.
    */
  def fromYaml(yamlString: String): Option[Manifest] = {
    yaml.parser.parse(yamlString).flatMap(_.as[Manifest]) match {
      case Left(error) =>
        Logger.debug(s"Error parsing the manifest: $error")
        None
      case Right(value) => Some(value)
    }
  }

  private object Fields {
    val minimumLauncherVersion = "minimum-launcher-version"
    val graalVMVersion         = "graal-vm-version"
    val graalJavaVersion       = "graal-java-version"
  }

  implicit private val semverDecoder: Decoder[SemVer] = { json =>
    for {
      string <- json.as[String]
      version <- SemVer(string).toRight(
        DecodingFailure(
          s"`$string` is not a valid semver version.",
          json.history
        )
      )
    } yield version
  }

  implicit private val decoder: Decoder[Manifest] = { json =>
    for {
      minimumLauncherVersion <- json.get[SemVer](Fields.minimumLauncherVersion)
      graalVMVersion         <- json.get[SemVer](Fields.graalVMVersion)
      graalJavaVersion <-
        json
          .get[String](Fields.graalJavaVersion)
          .orElse(json.get[Int](Fields.graalJavaVersion).map(_.toString))
    } yield Manifest(
      minimumLauncherVersion = minimumLauncherVersion,
      graalVMVersion         = graalVMVersion,
      graalJavaVersion       = graalJavaVersion
    )
  }
}
