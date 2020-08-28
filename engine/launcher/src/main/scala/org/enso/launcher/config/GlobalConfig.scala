package org.enso.launcher.config

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}

case class GlobalConfig(defaultVersion: DefaultVersion, original: JsonObject)

object GlobalConfig {
  val Default: GlobalConfig =
    GlobalConfig(
      defaultVersion = DefaultVersion.LatestInstalled,
      original       = JsonObject()
    )

  object Fields {
    val DefaultVersion = "default-enso-version"

    val nullable = Seq(DefaultVersion)
  }

  implicit val decoder: Decoder[GlobalConfig] = { json =>
    for {
      defaultVersion <- json.get[DefaultVersion](Fields.DefaultVersion)
      original       <- json.as[JsonObject]
    } yield GlobalConfig(defaultVersion, original)
  }

  implicit val encoder: Encoder[GlobalConfig] = { config =>
    val base = config.original.asJson

    val overrides =
      Json.obj(Fields.DefaultVersion -> config.defaultVersion.asJson)
    base.deepMerge(overrides).asJson
  }
}
