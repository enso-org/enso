package org.enso.pkg

import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.{yaml, Decoder, Encoder, Json}
import io.circe.yaml.Printer

import scala.util.Try

case class Dependency(name: String, version: String)

/**
  * Represents a package configuration stored in the `package.yaml` file.
  *
  * @param name package name
  * @param version package version
  * @param ensoVersion version of the Enso engine associated with the package,
  *                    can be set to `default` which defaults to the locally
  *                    installed version
  * @param license package license
  * @param author name and contact information of the package author(s)
  * @param maintainer name and contact information of current package
  *                   maintainer(s)
  * @param dependencies a list of package dependencies
  */
case class Config(
  name: String,
  version: String,
  ensoVersion: EnsoVersion,
  license: String,
  author: List[String],
  maintainer: List[String],
  dependencies: List[Dependency]
) {

  /**
    * Converts the configuration into a YAML representation.
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
    val author: String       = "author"
    val maintainer: String   = "maintainer"
    val dependencies: String = "dependencies"
  }

  private val decodeContactsList: Decoder[List[String]] = { json =>
    json
      .as[String]
      .map(name => if (name.isEmpty) List() else List(name))
      .orElse(json.as[List[String]])
  }

  private val encodeContactsList: Encoder[List[String]] = {
    case List()     => "".asJson
    case List(elem) => elem.asJson
    case l          => l.asJson
  }

  implicit val decoder: Decoder[Config] = { json =>
    for {
      name    <- json.get[String](JsonFields.name)
      version <- json.getOrElse[String](JsonFields.version)("dev")
      ensoVersion <-
        json.getOrElse[EnsoVersion](JsonFields.ensoVersion)(DefaultEnsoVersion)
      license <- json.getOrElse(JsonFields.license)("")
      author <- json.getOrElse[List[String]](JsonFields.author)(List())(
        decodeContactsList
      )
      maintainer <- json.getOrElse[List[String]](JsonFields.maintainer)(List())(
        decodeContactsList
      )
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
      dependencies
    )
  }

  implicit val encoder: Encoder[Config] = { config =>
    val base = Json.obj(
      JsonFields.name        -> config.name.asJson,
      JsonFields.version     -> config.version.asJson,
      JsonFields.ensoVersion -> config.ensoVersion.asJson,
      JsonFields.license     -> config.license.asJson,
      JsonFields.author      -> encodeContactsList(config.author),
      JsonFields.maintainer  -> encodeContactsList(config.maintainer)
    )
    val withDeps =
      if (config.dependencies.nonEmpty)
        base.deepMerge(
          Json.obj(JsonFields.dependencies -> config.dependencies.asJson)
        )
      else base
    withDeps
  }

  def fromYaml(yamlString: String): Try[Config] = {
    yaml.parser.parse(yamlString).flatMap(_.as[Config]).toTry
  }
}
