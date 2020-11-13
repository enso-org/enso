package org.enso.pkg

import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.{yaml, Decoder, DecodingFailure, Encoder, Json}
import io.circe.yaml.Printer

import scala.util.Try

/** An extra project dependency.
  * @param name name of the package
  * @param version package version
  */
case class Dependency(name: String, version: String)

/** Contact information to a user.
  *
  * Used for defining authors and maintainers.
  * At least one of the fields must not be None.
  * @param name contact name
  * @param email contact email
  */
case class Contact(name: Option[String], email: Option[String]) {
  if (name.isEmpty && email.isEmpty)
    throw new IllegalArgumentException(
      "At least one of fields `name` or `email` must be defined."
    )

  /** @inheritdoc
    */
  override def toString: String = {
    val space = if (name.isDefined && email.isDefined) " " else ""
    name.getOrElse("") + space + email.map(email => s"<$email>").getOrElse("")
  }
}
object Contact {

  /** Fields for use when serializing the [[Contact]].
    */
  object Fields {
    val Name  = "name"
    val Email = "email"
  }

  /** [[Encoder]] instance for the [[Contact]].
    */
  implicit val encoder: Encoder[Contact] = { contact =>
    val name  = contact.name.map(Fields.Name -> _.asJson)
    val email = contact.email.map(Fields.Email -> _.asJson)
    Json.obj((name.toSeq ++ email.toSeq): _*)
  }

  /** [[Decoder]] instance for the [[Contact]].
    */
  implicit val decoder: Decoder[Contact] = { json =>
    def verifyAtLeastOneDefined(
      name: Option[String],
      email: Option[String]
    ): Either[DecodingFailure, Unit] =
      if (name.isEmpty && email.isEmpty)
        Left(
          DecodingFailure(
            "At least one of the fields `name`, `email` must be defined.",
            json.history
          )
        )
      else Right(())

    for {
      name  <- json.getOrElse[Option[String]](Fields.Name)(None)
      email <- json.getOrElse[Option[String]](Fields.Email)(None)
      _     <- verifyAtLeastOneDefined(name, email)
    } yield Contact(name, email)
  }
}

/** Represents a package configuration stored in the `package.yaml` file.
  *
  * @param name package name
  * @param version package version
  * @param ensoVersion version of the Enso engine associated with the package,
  *                    can be set to `default` which defaults to the locally
  *                    installed version
  * @param license package license
  * @param authors name and contact information of the package author(s)
  * @param maintainers name and contact information of current package
  *                   maintainer(s)
  * @param dependencies a list of package dependencies
  * @param originalJson a Json object holding the original values that this
  *                     Config was created from, used to preserve configuration
  *                     keys that are not known
  */
case class Config(
  name: String,
  version: String,
  ensoVersion: EnsoVersion,
  license: String,
  authors: List[Contact],
  maintainers: List[Contact],
  dependencies: List[Dependency],
  originalJson: Json = Json.obj()
) {

  /** Converts the configuration into a YAML representation.
    */
  def toYaml: String =
    Printer.spaces2.copy(preserveOrder = true).pretty(Config.encoder(this))
}

object Config {
  private object JsonFields {
    val name: String         = "name"
    val version: String      = "version"
    val ensoVersion: String  = "enso-version"
    val license: String      = "license"
    val author: String       = "authors"
    val maintainer: String   = "maintainers"
    val dependencies: String = "dependencies"
  }

  implicit val decoder: Decoder[Config] = { json =>
    for {
      name    <- json.get[String](JsonFields.name)
      version <- json.getOrElse[String](JsonFields.version)("dev")
      ensoVersion <-
        json.getOrElse[EnsoVersion](JsonFields.ensoVersion)(DefaultEnsoVersion)
      license    <- json.getOrElse(JsonFields.license)("")
      author     <- json.getOrElse[List[Contact]](JsonFields.author)(List())
      maintainer <- json.getOrElse[List[Contact]](JsonFields.maintainer)(List())
      dependencies <- json.getOrElse[List[Dependency]](JsonFields.dependencies)(
        List()
      )
    } yield Config(
      name,
      version,
      ensoVersion,
      license,
      author,
      maintainer,
      dependencies,
      json.value
    )
  }

  implicit val encoder: Encoder[Config] = { config =>
    val originals = config.originalJson
    val overrides = Json.obj(
      JsonFields.name        -> config.name.asJson,
      JsonFields.version     -> config.version.asJson,
      JsonFields.ensoVersion -> config.ensoVersion.asJson,
      JsonFields.license     -> config.license.asJson,
      JsonFields.author      -> config.authors.asJson,
      JsonFields.maintainer  -> config.maintainers.asJson
    )
    val base = originals.deepMerge(overrides)
    val withDeps =
      if (config.dependencies.nonEmpty)
        base.deepMerge(
          Json.obj(JsonFields.dependencies -> config.dependencies.asJson)
        )
      else base
    withDeps
  }

  /** Tries to parse the [[Config]] from a YAML string.
    */
  def fromYaml(yamlString: String): Try[Config] = {
    yaml.parser.parse(yamlString).flatMap(_.as[Config]).toTry
  }
}
