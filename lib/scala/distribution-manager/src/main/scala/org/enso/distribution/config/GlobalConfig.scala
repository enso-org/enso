package org.enso.distribution.config

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}
import org.enso.distribution.config

/** Global user configuration.
  *
  * Specifies default values for new projects and possibly other properties. Can
  * handle unknown keys which can be used for configuration of plugins.
  *
  * @param defaultVersion default Enso version for launching Enso outside of
  *                       projects and creating new projects
  * @param authorName default author (and maintainer) name for newly created
  *                   projects
  * @param authorEmail default author (and maintainer) email for newly created
  *                    projects
  * @param editionProviders a sequence of edition provider URLs that are used
  *                         for downloading editions
  * @param original original mapping that may contain unknown keys
  */
case class GlobalConfig(
  defaultVersion: DefaultVersion,
  authorName: Option[String],
  authorEmail: Option[String],
  editionProviders: Seq[String],
  original: JsonObject
)

object GlobalConfig {
  private val defaultEditionProviders: Seq[String] = Seq(
    "https://editions.release.enso.org/enso"
  )

  /** The default configuration used when the configuration file does not exist.
    */
  val Default: GlobalConfig =
    GlobalConfig(
      defaultVersion   = DefaultVersion.LatestInstalled,
      authorName       = None,
      authorEmail      = None,
      editionProviders = defaultEditionProviders,
      original         = JsonObject()
    )

  /** Field names used when serializing the configuration.
    */
  object Fields {
    val DefaultVersion   = "default.enso-version"
    val AuthorName       = "author.name"
    val AuthorEmail      = "author.email"
    val EditionProviders = "edition-providers"
  }

  /** [[Decoder]] instance for [[GlobalConfig]].
    */
  implicit val decoder: Decoder[GlobalConfig] = { json =>
    for {
      defaultVersion <- json.getOrElse[DefaultVersion](Fields.DefaultVersion)(
        DefaultVersion.LatestInstalled
      )
      authorName  <- json.getOrElse[Option[String]](Fields.AuthorName)(None)
      authorEmail <- json.getOrElse[Option[String]](Fields.AuthorEmail)(None)
      editionProviders <- json.getOrElse[Seq[String]](Fields.EditionProviders)(
        defaultEditionProviders
      )
      original <- json.as[JsonObject]
    } yield config.GlobalConfig(
      defaultVersion   = defaultVersion,
      authorName       = authorName,
      authorEmail      = authorEmail,
      editionProviders = editionProviders,
      original         = original
    )
  }

  /** [[Encoder]] instance for [[GlobalConfig]].
    */
  implicit val encoder: Encoder[GlobalConfig] = { config =>
    val base = config.original.asJson

    val overrides =
      Json.obj(
        Fields.DefaultVersion   -> config.defaultVersion.asJson,
        Fields.AuthorName       -> config.authorName.asJson,
        Fields.AuthorEmail      -> config.authorEmail.asJson,
        Fields.EditionProviders -> config.editionProviders.asJson
      )
    base.deepMerge(overrides).dropNullValues.asJson
  }
}
