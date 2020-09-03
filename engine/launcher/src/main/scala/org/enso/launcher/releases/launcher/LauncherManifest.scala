package org.enso.launcher.releases.launcher

import io.circe.{yaml, Decoder}
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.releases.ReleaseProviderException
import org.enso.pkg.SemVerJson._

import scala.util.{Failure, Try}

case class LauncherManifest(
  minimumVersionToUpgrade: SemVer,
  filesToCopy: Seq[String],
  directoriesToCopy: Seq[String]
)

object LauncherManifest {
  val assetName: String = "launcher-manifest.yaml"
  object Fields {
    val minimumVersionToUpgrade = "minimum-version-to-upgrade"
    val filesToCopy             = "files-to-copy"
    val directoriesToCopy       = "directories-to-copy"
  }
  implicit val decoder: Decoder[LauncherManifest] = { json =>
    for {
      minimumVersionToUpgrade <-
        json.get[SemVer](Fields.minimumVersionToUpgrade)
      files <- json.getOrElse[Seq[String]](Fields.filesToCopy)(Seq())
      directories <-
        json.getOrElse[Seq[String]](Fields.directoriesToCopy)(Seq())
    } yield LauncherManifest(
      minimumVersionToUpgrade = minimumVersionToUpgrade,
      filesToCopy             = files,
      directoriesToCopy       = directories
    )
  }

  def fromYAML(string: String): Try[LauncherManifest] =
    yaml.parser
      .parse(string)
      .flatMap(_.as[LauncherManifest])
      .toTry
      .recoverWith { error =>
        // TODO [RW] more readable errors in #1111
        Failure(
          ReleaseProviderException(
            s"Cannot parse launcher manifest: $error.",
            error
          )
        )
      }
}
