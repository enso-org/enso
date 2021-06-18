package org.enso.launcher.releases.launcher

import io.circe.{yaml, Decoder}
import nl.gn0s1s.bump.SemVer
import org.enso.runtimeversionmanager.releases.ReleaseProviderException
import org.enso.editions.SemVerJson._

import scala.util.{Failure, Try}

/** Contains release metadata associated with a launcher release.
  *
  * @param minimumVersionForUpgrade minimum version of the current launcher that
  *                                is required to upgrade to this version; if
  *                                current launcher is older than that provided
  *                                version, a multi-step upgrade must be
  *                                performed
  * @param filesToCopy a sequence of filenames of files that should be updated
  *                    in the data root
  * @param directoriesToCopy a sequence of names of directories that should be
  *                          updated in the data root
  */
case class LauncherManifest(
  minimumVersionForUpgrade: SemVer,
  filesToCopy: Seq[String],
  directoriesToCopy: Seq[String]
)

object LauncherManifest {

  /** Default name of the asset containing the launcher manifest.
    */
  val assetName: String = "launcher-manifest.yaml"

  private object Fields {
    val minimumVersionForUpgrade = "minimum-version-for-upgrade"
    val filesToCopy              = "files-to-copy"
    val directoriesToCopy        = "directories-to-copy"
  }

  /** [[Decoder]] instance for [[LauncherManifest]].
    */
  implicit val decoder: Decoder[LauncherManifest] = { json =>
    for {
      minimumVersionToUpgrade <-
        json.get[SemVer](Fields.minimumVersionForUpgrade)
      files <- json.getOrElse[Seq[String]](Fields.filesToCopy)(Seq())
      directories <-
        json.getOrElse[Seq[String]](Fields.directoriesToCopy)(Seq())
    } yield LauncherManifest(
      minimumVersionForUpgrade = minimumVersionToUpgrade,
      filesToCopy              = files,
      directoriesToCopy        = directories
    )
  }

  /** Tries to parse the [[LauncherManifest]] from a [[String]].
    */
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
