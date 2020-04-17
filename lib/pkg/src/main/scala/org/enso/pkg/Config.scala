package org.enso.pkg

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.yaml
import io.circe.yaml.syntax._

case class Config(
  author: String,
  maintainer: String,
  name: String,
  version: String,
  license: String
) {
  def toYaml: String = this.asJson.asYaml.spaces4
}

object Config {
  def fromYaml(yamlString: String): Option[Config] = {
    yaml.parser.parse(yamlString).flatMap(_.as[Config]).toOption
  }

}
