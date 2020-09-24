package org.enso.loggingservice.internal.protocol

import java.time.Instant

import io.circe.syntax._
import io.circe.{Decoder, Encoder, JsonObject}
import org.enso.loggingservice.LogLevel

case class WSLogMessage(
  logLevel: LogLevel,
  timestamp: Instant,
  group: String,
  message: String,
  exception: Option[SerializedException]
)

object WSLogMessage {
  object JsonFields {
    val Level     = "level"
    val Timestamp = "time"
    val Group     = "group"
    val Message   = "message"
    val Exception = "exception"
  }
  implicit val encoder: Encoder[WSLogMessage] = { message =>
    val base = JsonObject(
      JsonFields.Level     -> message.logLevel.asJson,
      JsonFields.Timestamp -> message.timestamp.toEpochMilli.asJson,
      JsonFields.Group     -> message.group.asJson,
      JsonFields.Message   -> message.message.asJson
    )

    val result = message.exception match {
      case Some(exception) =>
        base.+:((JsonFields.Exception, exception.asJson))
      case None =>
        base
    }

    result.asJson
  }

  implicit val decoder: Decoder[WSLogMessage] = { json =>
    for {
      level <- json.get[LogLevel](JsonFields.Level)
      timestamp <-
        json.get[Long](JsonFields.Timestamp).map(Instant.ofEpochMilli)
      group   <- json.get[String](JsonFields.Group)
      message <- json.get[String](JsonFields.Message)
      exception <-
        json.getOrElse[Option[SerializedException]](JsonFields.Exception)(None)
    } yield WSLogMessage(
      logLevel  = level,
      timestamp = timestamp,
      group     = group,
      message   = message,
      exception = exception
    )
  }
}
