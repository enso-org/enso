package org.enso.loggingservice

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}

sealed abstract class LogLevel(final val level: Int) {
  def shouldLog(other: LogLevel): Boolean =
    other.level <= level
}
object LogLevel {
  case object Off extends LogLevel(-1) {
    override def toString: String = "off"
  }
  case object Error extends LogLevel(0) {
    override def toString: String = "error"
  }
  case object Warning extends LogLevel(1) {
    override def toString: String = "warning"
  }
  case object Info extends LogLevel(2) {
    override def toString: String = "info"
  }
  case object Debug extends LogLevel(3) {
    override def toString: String = "debug"
  }
  case object Trace extends LogLevel(4) {
    override def toString: String = "trace"
  }

  val allLevels = Seq(
    LogLevel.Off,
    LogLevel.Error,
    LogLevel.Warning,
    LogLevel.Info,
    LogLevel.Debug,
    LogLevel.Trace
  )

  implicit val ord: Ordering[LogLevel] = (x, y) => x.level - y.level

  implicit val encoder: Encoder[LogLevel] = {
    case Off =>
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
