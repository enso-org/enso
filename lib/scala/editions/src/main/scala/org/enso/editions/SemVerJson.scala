package org.enso.editions

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}

import scala.util.{Success, Try}
import com.github.zafarkhaja.semver.Version

object SemVerJson {

  /** [[Decoder]] instance allowing to parse semantic versioning strings.
    */
  implicit val semverDecoder: Decoder[Version] = { json =>
    for {
      string  <- json.as[String]
      version <- safeParse(string, json)
    } yield version
  }

  private def safeParse(
    version: String,
    json: HCursor
  ): Either[DecodingFailure, Version] = {
    Try(Version.parse(version)) match {
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
  implicit val semverEncoder: Encoder[Version] = _.toString.asJson
}
