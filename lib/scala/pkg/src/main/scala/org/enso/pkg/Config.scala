package org.enso.pkg

import io.circe.syntax._
import io.circe.generic.auto._
import io.circe.{yaml, Decoder, Encoder, Json}
import io.circe.yaml.Printer

case class Dependency(name: String, version: String)

case class Config(
  name: String,
  version: String,
  license: String,
  author: List[String],
  maintainer: List[String],
  dependencies: List[Dependency]
) {
  def toYaml: String =
    Printer.spaces2.copy(preserveOrder = true).pretty(Config.encoder(this))
}

object Config {
  private object JsonFields {
    val name: String         = "name"
    val version: String      = "version"
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
      name     <- json.get[String](JsonFields.name)
      version  <- json.get[String](JsonFields.version)
      license  <- json.getOrElse(JsonFields.license)("")
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
      license,
      author,
      maintainer,
      dependencies
    )
  }

  implicit val encoder: Encoder[Config] = { config =>
    val base = Json.obj(
      JsonFields.name       -> config.name.asJson,
      JsonFields.version    -> config.version.asJson,
      JsonFields.license    -> config.license.asJson,
      JsonFields.author     -> encodeContactsList(config.author),
      JsonFields.maintainer -> encodeContactsList(config.maintainer)
    )
    val withDeps =
      if (config.dependencies.nonEmpty)
        base.deepMerge(
          Json.obj(JsonFields.dependencies -> config.dependencies.asJson)
        )
      else base
    withDeps
  }

  def fromYaml(yamlString: String): Option[Config] = {
    yaml.parser.parse(yamlString).flatMap(_.as[Config]).toOption
  }
}
