package org.enso.loggingservice.internal

import java.sql.Timestamp
import java.time.Instant

import io.circe.syntax._
import io.circe.{Decoder, Encoder, JsonObject}

case class WSLogMessage(
  logLevel: Level,
  timestamp: Timestamp,
  group: String,
  message: String,
  exception: Option[SerializedException]
)

object WSLogMessage {
  def apply(
    logLevel: Level,
    group: String,
    message: String,
    exception: Option[SerializedException]
  ): WSLogMessage = {
    val now = Timestamp.from(Instant.now())
    WSLogMessage(
      logLevel  = logLevel,
      timestamp = now,
      group     = group,
      message   = message,
      exception = exception
    )
  }

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
      JsonFields.Timestamp -> message.timestamp.getTime.asJson,
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
      level     <- json.get[Level](JsonFields.Level)
      timestamp <- json.get[Long](JsonFields.Timestamp).map(new Timestamp(_))
      group     <- json.get[String](JsonFields.Group)
      message   <- json.get[String](JsonFields.Message)
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
