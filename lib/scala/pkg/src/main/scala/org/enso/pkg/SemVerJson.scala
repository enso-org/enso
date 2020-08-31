package org.enso.pkg

import io.circe.{Decoder, DecodingFailure, Encoder}
import io.circe.syntax._
import nl.gn0s1s.bump.SemVer

object SemVerJson {

  /**
    * [[Decoder]] instance allowing to parse semantic versioning strings.
    */
  implicit val semverDecoder: Decoder[SemVer] = { json =>
    for {
      string <- json.as[String]
      version <- SemVer(string).toRight(
        DecodingFailure(
          s"`$string` is not a valid semantic versioning string.",
          json.history
        )
      )
    } yield version
  }

  /**
    * [[Encoder]] instance allowing to serialize semantic versioning strings.
    */
  implicit val semverEncoder: Encoder[SemVer] = _.toString.asJson
}
