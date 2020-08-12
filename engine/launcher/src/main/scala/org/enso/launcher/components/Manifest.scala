package org.enso.launcher.components

import java.io.FileReader
import java.nio.file.Path

import io.circe.{yaml, Decoder, DecodingFailure}
import nl.gn0s1s.bump.SemVer

import scala.util.{Failure, Try, Using}

/**
  * Version information identifying the runtime that can be used with an engine
  * release.
  *
  * @param graal version of the GraalVM
  * @param java Java version of the GraalVM flavour that should be used
  */
case class RuntimeVersion(graal: SemVer, java: String) {

  /**
    * @inheritdoc
    */
  override def toString: String = s"GraalVM $graal Java $java"
}

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
  def load(path: Path): Try[Manifest] =
    Using(new FileReader(path.toFile)) { reader =>
      yaml.parser
        .parse(reader)
        .flatMap(_.as[Manifest])
        .toTry
        .recoverWith { error => Failure(ManifestLoadingError(error)) }
    }.flatten

  /**
    * Parses the manifest from a string containing a YAML definition.
    *
    * Returns None if the definition cannot be parsed.
    */
  def fromYaml(yamlString: String): Try[Manifest] = {
    yaml.parser
      .parse(yamlString)
      .flatMap(_.as[Manifest])
      .toTry
      .recoverWith { error => Failure(ManifestLoadingError(error)) }
  }

  case class ManifestLoadingError(cause: Throwable)
      extends RuntimeException(s"Could not load the manifest: $cause", cause)

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
