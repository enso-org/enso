package org.enso.loggingservice.internal.protocol

import java.time.Instant

import io.circe.syntax._
import io.circe.{Decoder, Encoder, JsonObject}
import org.enso.loggingservice.LogLevel
import org.enso.loggingservice.internal.BaseLogMessage

/** The encoded log message that can be sent over the WebSocket connection or
  * passed to a printer.
  *
  * @param level log level
  * @param timestamp timestamp indicating when the message was created
  * @param group group associated with the message
  * @param message text message
  * @param exception optional serialized exception attached to the message
  */
case class WSLogMessage(
  level: LogLevel,
  timestamp: Instant,
  group: String,
  message: String,
  exception: Option[SerializedException]
) extends BaseLogMessage[SerializedException]

object WSLogMessage {
  private object JsonFields {
    val Level     = "level"
    val Timestamp = "time"
    val Group     = "group"
    val Message   = "message"
    val Exception = "exception"
  }

  /** [[Encoder]] instance for [[WSLogMessage]].
    */
  implicit val encoder: Encoder[WSLogMessage] = { message =>
    var base = JsonObject(
      JsonFields.Level     -> message.level.asJson,
      JsonFields.Timestamp -> message.timestamp.toEpochMilli.asJson
    )

    if (message.group != null) {
      base = base.+:((JsonFields.Group -> message.group.asJson))
    }
    if (message.message != null) {
      base = base.+:((JsonFields.Message -> message.message.asJson))
    }

    val result = message.exception match {
      case Some(exception) =>
        base.+:((JsonFields.Exception, exception.asJson))
      case None =>
        base
    }

    result.asJson
  }

  /** [[Decoder]] instance for [[WSLogMessage]].
    */
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
      level     = level,
      timestamp = timestamp,
      group     = group,
      message   = message,
      exception = exception
    )
  }
}
