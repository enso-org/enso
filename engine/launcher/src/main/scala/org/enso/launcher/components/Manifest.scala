package org.enso.launcher.components

import java.io.{FileReader, IOException}
import java.nio.file.Path

import io.circe.{yaml, Decoder, DecodingFailure}
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.Logger

case class RuntimeVersion(graal: SemVer, java: String)
case class Manifest(
  minimumLauncherVersion: SemVer,
  graalVMVersion: SemVer,
  graalJavaVersion: String
) {
  def runtimeVersion: RuntimeVersion =
    RuntimeVersion(graalVMVersion, graalJavaVersion)
}

object Manifest {
  val DEFAULT_MANIFEST_NAME = "manifest.yaml"
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

  implicit val semverDecoder: Decoder[SemVer] = { json =>
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

  implicit val decoder: Decoder[Manifest] = { json =>
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
