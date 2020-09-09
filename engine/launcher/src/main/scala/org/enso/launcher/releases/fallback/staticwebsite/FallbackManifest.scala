package org.enso.launcher.releases.fallback.staticwebsite

import io.circe.Decoder

import scala.util.Try

case class FallbackManifest(enabled: Boolean)

object FallbackManifest {

  val fileName = "fallback-manifest.yaml"

  private object Fields {
    val enabled = "enabled"
  }

  implicit val decoder: Decoder[FallbackManifest] = { json =>
    for {
      enabled <- json.get[Boolean](Fields.enabled)
    } yield FallbackManifest(enabled)
  }

  def parseYAML(string: String): Try[FallbackManifest] =
    io.circe.yaml.parser.parse(string).flatMap(_.as[FallbackManifest]).toTry
}
