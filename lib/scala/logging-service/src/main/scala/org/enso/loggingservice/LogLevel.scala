package org.enso.loggingservice

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}

sealed class LogLevel(final val level: Int) {
  def shouldLog(other: LogLevel): Boolean =
    other.level <= level
}
object LogLevel {
  case object None    extends LogLevel(-1)
  case object Error   extends LogLevel(0)
  case object Warning extends LogLevel(1)
  case object Info    extends LogLevel(2)
  case object Debug   extends LogLevel(3)
  case object Trace   extends LogLevel(4)

  val allLevels = Seq(
    LogLevel.None,
    LogLevel.Error,
    LogLevel.Warning,
    LogLevel.Info,
    LogLevel.Debug,
    LogLevel.Trace
  )

  implicit val ord: Ordering[LogLevel] = (x, y) => x.level - y.level

  implicit val encoder: Encoder[LogLevel] = {
    case None =>
      throw new IllegalArgumentException(
        "`None` log level should never be used in actual log messages and it " +
        "cannot be serialized to prevent that."
      )
    case level =>
      level.level.asJson
  }

  implicit val decoder: Decoder[LogLevel] = { json =>
    json.as[Int].flatMap {
      case Error.level   => Right(Error)
      case Warning.level => Right(Warning)
      case Info.level    => Right(Info)
      case Debug.level   => Right(Debug)
      case Trace.level   => Right(Trace)
      case other =>
        Left(
          DecodingFailure(s"`$other` is not a valid log level.", json.history)
        )
    }
  }
}
