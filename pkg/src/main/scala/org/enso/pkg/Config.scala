package org.enso.pkg

import java.io.File
import java.io.PrintWriter

import io.circe.yaml
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.yaml.syntax._

import scala.io.Source
import scala.util.Try

case class Config(
    author: String,
    maintainer: String,
    name: String,
    version: String,
    license: String) {
  def toYaml: String = this.asJson.asYaml.spaces4
}

object Config {
  def fromYaml(yamlString: String): Option[Config] = {
    yaml.parser.parse(yamlString).flatMap(_.as[Config]).toOption
  }

}
