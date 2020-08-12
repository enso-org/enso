package org.enso.pkg

import io.circe.{Decoder, DecodingFailure}
import nl.gn0s1s.bump.SemVer

object SemVerDecoder {
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
}
