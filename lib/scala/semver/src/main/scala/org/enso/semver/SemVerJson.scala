package org.enso.semver

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}

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
            json.history
          )
        )
    }
  }

  /** [[Encoder]] instance allowing to serialize semantic versioning strings.
    */
  implicit val semverEncoder: Encoder[SemVer] = _.toString.asJson
}
