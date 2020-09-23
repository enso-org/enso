package org.enso.loggingservice.internal

import java.sql.Timestamp

import io.circe.{Decoder, Encoder}

case class WSLogMessage(
  logLevel: Level,
  timestamp: Timestamp,
  component: String,
  message: String,
  exception: Option[SerializedException]
)

object WSLogMessage {
  implicit val encoder: Encoder[WSLogMessage] = { message =>
    ???
  }

  implicit val decoder: Decoder[WSLogMessage] = { json =>
    ???
  }
}
