package org.enso.launcher.components

import java.io.FileReader
import java.nio.file.Path

import io.circe.{yaml, Decoder}
import nl.gn0s1s.bump.SemVer

import org.enso.pkg.SemVerDecoder._

import scala.util.{Failure, Try, Using}

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
  * @param jvmOptions a list of JVM options that should be added when running
  *                   this engine
  */
case class Manifest(
  minimumLauncherVersion: SemVer,
  graalVMVersion: SemVer,
  graalJavaVersion: String,
  jvmOptions: Seq[(String, String)]
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
    val jvmOptions             = "jvm-options"
    val graalVMVersion         = "graal-vm-version"
    val graalJavaVersion       = "graal-java-version"
  }

  implicit private val optsDecoder: Decoder[Seq[(String, String)]] = { json =>
    json.as[Map[String, String]].map(_.toSeq)
  }

  implicit private val decoder: Decoder[Manifest] = { json =>
    for {
      minimumLauncherVersion <- json.get[SemVer](Fields.minimumLauncherVersion)
      graalVMVersion         <- json.get[SemVer](Fields.graalVMVersion)
      graalJavaVersion <-
        json
          .get[String](Fields.graalJavaVersion)
          .orElse(json.get[Int](Fields.graalJavaVersion).map(_.toString))
      jvmOptions <-
        json.getOrElse[Seq[(String, String)]](Fields.jvmOptions)(Seq())
    } yield Manifest(
      minimumLauncherVersion = minimumLauncherVersion,
      graalVMVersion         = graalVMVersion,
      graalJavaVersion       = graalJavaVersion,
      jvmOptions             = jvmOptions
    )
  }
}
