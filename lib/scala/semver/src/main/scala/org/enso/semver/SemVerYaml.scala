package org.enso.semver

import org.enso.scala.yaml.{YamlDecoder, YamlEncoder}
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{Node, ScalarNode}

import scala.util.{Failure, Success}

object SemVerYaml {

  private def safeParse(version: String): Either[YAMLException, SemVer] = {
    SemVer.parse(version) match {
      case Success(parsed) => Right(parsed)
      case Failure(throwable) =>
        Left(
          new YAMLException(
            s"`$version` is not a valid semantic versioning string.",
            throwable
          )
        )
    }
  }
  implicit val yamlSemverDecoder: YamlDecoder[SemVer] =
    new YamlDecoder[SemVer] {
      override def decode(node: Node): Either[Throwable, SemVer] = node match {
        case node: ScalarNode =>
          safeParse(node.getValue)
        case _ =>
          Left(
            new YAMLException(
              "Expected a simple value node for SemVer, instead got " + node.getClass
            )
          )
      }
    }

  implicit val yamlSemverEncoder: YamlEncoder[SemVer] =
    new YamlEncoder[SemVer] {
      override def encode(value: SemVer): Object = {
        value.toString
      }
    }
}
