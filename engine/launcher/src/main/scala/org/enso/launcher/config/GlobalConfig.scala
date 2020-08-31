package org.enso.launcher.config

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}
import org.enso.pkg.Contact

case class GlobalConfig(
  defaultVersion: DefaultVersion,
  authorName: Option[String],
  authorEmail: Option[String],
  original: JsonObject
) {
  def defaultAuthor: Option[Contact] =
    if (authorName.isEmpty && authorEmail.isEmpty) None
    else Some(Contact(name = authorName, email = authorEmail))
}

object GlobalConfig {
  val Default: GlobalConfig =
    GlobalConfig(
      defaultVersion = DefaultVersion.LatestInstalled,
      authorName     = None,
      authorEmail    = None,
      original       = JsonObject()
    )

  object Fields {
    val DefaultVersion = "default.enso-version"
    val AuthorName     = "author.name"
    val AuthorEmail    = "author.email"
  }

  implicit val decoder: Decoder[GlobalConfig] = { json =>
    for {
      defaultVersion <- json.getOrElse[DefaultVersion](Fields.DefaultVersion)(
        DefaultVersion.LatestInstalled
      )
      authorName  <- json.getOrElse[Option[String]](Fields.AuthorName)(None)
      authorEmail <- json.getOrElse[Option[String]](Fields.AuthorEmail)(None)
      original    <- json.as[JsonObject]
    } yield GlobalConfig(
      defaultVersion = defaultVersion,
      authorName     = authorName,
      authorEmail    = authorEmail,
      original       = original
    )
  }

  implicit val encoder: Encoder[GlobalConfig] = { config =>
    val base = config.original.asJson

    val overrides =
      Json.obj(
        Fields.DefaultVersion -> config.defaultVersion.asJson,
        Fields.AuthorName     -> config.authorName.asJson,
        Fields.AuthorEmail    -> config.authorEmail.asJson
      )
    base.deepMerge(overrides).asJson
  }
}
