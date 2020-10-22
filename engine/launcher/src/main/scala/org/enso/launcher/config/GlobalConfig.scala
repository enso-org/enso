package org.enso.launcher.config

import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}
import org.enso.pkg.Contact

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
  * @param original original mapping that may contain unknown keys
  */
case class GlobalConfig(
  defaultVersion: DefaultVersion,
  authorName: Option[String],
  authorEmail: Option[String],
  original: JsonObject
) {

  /** Returns a [[Contact]] for the default author if at least one of the name
    * and email is set.
    *
    * If both name and email are not set, returns None.
    */
  def defaultAuthor: Option[Contact] =
    if (authorName.isEmpty && authorEmail.isEmpty) None
    else Some(Contact(name = authorName, email = authorEmail))
}

object GlobalConfig {

  /** The default configuration used when the configuration file does not exist.
    */
  val Default: GlobalConfig =
    GlobalConfig(
      defaultVersion = DefaultVersion.LatestInstalled,
      authorName     = None,
      authorEmail    = None,
      original       = JsonObject()
    )

  /** Field names used when serializing the configuration.
    */
  object Fields {
    val DefaultVersion = "default.enso-version"
    val AuthorName     = "author.name"
    val AuthorEmail    = "author.email"
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
      original    <- json.as[JsonObject]
    } yield GlobalConfig(
      defaultVersion = defaultVersion,
      authorName     = authorName,
      authorEmail    = authorEmail,
      original       = original
    )
  }

  /** [[Encoder]] instance for [[GlobalConfig]].
    */
  implicit val encoder: Encoder[GlobalConfig] = { config =>
    val base = config.original.asJson

    val overrides =
      Json.obj(
        Fields.DefaultVersion -> config.defaultVersion.asJson,
        Fields.AuthorName     -> config.authorName.asJson,
        Fields.AuthorEmail    -> config.authorEmail.asJson
      )
    base.deepMerge(overrides).dropNullValues.asJson
  }
}
