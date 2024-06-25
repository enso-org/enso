package org.enso.semver

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}
import org.yaml.snakeyaml.error.YAMLException
import org.yaml.snakeyaml.nodes.{Node, ScalarNode}
import org.enso.yaml.SnakeYamlDecoder

import scala.util.Success

object SemVerJson {

  /** [[Decoder]] instance allowing to parse semantic versioning strings.
    */
  implicit val semverDecoder: Decoder[SemVer] = { json =>
    for {
      string  <- json.as[String]
      version <- safeParse(string, json)
    } yield version
  }

  private def safeParse(
    version: String,
    json: HCursor
  ): Either[DecodingFailure, SemVer] = {
    SemVer.parse(version) match {
      case Success(parsed) => Right(parsed)
      case _ =>
        Left(
          DecodingFailure(
            s"`$version` is not a valid semantic versioning string.",
            if (json != null) json.history else Nil
          )
        )
    }
  }

  /** [[Encoder]] instance allowing to serialize semantic versioning strings.
    */
  implicit val semverEncoder: Encoder[SemVer] = _.toString.asJson

  implicit val yamlDecoder: SnakeYamlDecoder[SemVer] =
    new SnakeYamlDecoder[SemVer] {
      override def decode(node: Node): Either[Throwable, SemVer] = node match {
        case node: ScalarNode =>
          safeParse(node.getValue, null).left.map(_.getCause)
        case _ =>
          Left(
            new YAMLException(
              "Expected a simple value node for SemVer, instead got " + node.getClass
            )
          )
      }
    }

}
