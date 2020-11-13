package org.enso.launcher.releases.fallback.staticwebsite

import io.circe.Decoder

import scala.util.Try

/** Manifest of the fallback mechanism.
  *
  * Specifies whether the mechanism is enabled and should be considered
  * available.
  */
case class FallbackManifest(enabled: Boolean)

object FallbackManifest {

  /** Defines a part of the URL scheme of the fallback mechanism - the name of
    * manifest file.
    *
    * That must *never* be changed to ensure that all older launcher versions
    * can be upgraded.
    */
  val fileName = "fallback-manifest.yaml"

  private object Fields {
    val enabled = "enabled"
  }

  /** [[Decoder]] instance for [[FallbackManifest]].
    *
    * It should always remain backwards compatible, since the fallback mechanism
    * must work for all released launcher versions.
    */
  implicit val decoder: Decoder[FallbackManifest] = { json =>
    for {
      enabled <- json.get[Boolean](Fields.enabled)
    } yield FallbackManifest(enabled)
  }

  def parseString(string: String): Try[FallbackManifest] =
    io.circe.yaml.parser.parse(string).flatMap(_.as[FallbackManifest]).toTry
}
