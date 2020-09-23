package org.enso.loggingservice.internal

import io.circe.{Decoder, DecodingFailure, Encoder}
import io.circe.syntax._

sealed class Level(final val level: Int)
object Level {
  case object None    extends Level(-1)
  case object Error   extends Level(0)
  case object Warning extends Level(1)
  case object Info    extends Level(2)
  case object Debug   extends Level(3)
  case object Trace   extends Level(4)

  implicit val ord: Ordering[Level] = (x, y) => x.level - y.level

  implicit val encoder: Encoder[Level] = {
    case None =>
      throw new IllegalArgumentException(
        "`None` log level should never be used in actual log messages and it " +
        "cannot be serialized to prevent that."
      )
    case level =>
      level.level.asJson
  }

  implicit val decoder: Decoder[Level] = { json =>
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
