package org.enso.launcher.releases.launcher

import io.circe.{yaml, Decoder}
import nl.gn0s1s.bump.SemVer
import org.enso.launcher.releases.ReleaseProviderException
import org.enso.pkg.SemVerJson._

import scala.util.{Failure, Try}

case class LauncherManifest(minimumVersionToUpgrade: SemVer)

object LauncherManifest {
  val assetName: String = "launcher-manifest.yaml"
  object Fields {
    val minimumVersionToUpgrade = "minimum-version-to-upgrade"
  }
  implicit val decoder: Decoder[LauncherManifest] = { json =>
    for {
      minimumVersionToUpgrade <-
        json.get[SemVer](Fields.minimumVersionToUpgrade)
    } yield LauncherManifest(minimumVersionToUpgrade)
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
