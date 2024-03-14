package org.enso.projectmanager.infrastructure.file

import io.circe.syntax._
import io.circe.{Decoder, Encoder}

import java.io.File

object FileJson {

  implicit val fileDecoder: Decoder[File] = { json =>
    json.as[String].map(new File(_))
  }

  implicit val fileEncoder: Encoder[File] = _.getAbsolutePath.asJson
}
