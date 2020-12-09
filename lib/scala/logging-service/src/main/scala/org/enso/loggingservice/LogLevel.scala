package org.enso.loggingservice

import io.circe.syntax._
import io.circe.{Decoder, DecodingFailure, Encoder}

/** Defines a log level for log messages.
  */
sealed abstract class LogLevel(final val level: Int) {

  /** Determines if a component running on `this` log level should log the
    * `other`.
    *
    * Log levels smaller or equal to component's log level are logged.
    */
  def shouldLog(other: LogLevel): Boolean =
    other.level <= level
}

object LogLevel {

  /** This log level should not be used by messages, instead it can be set as
    * component's log level to completely disable logging for it.
    */
  case object Off extends LogLevel(-1) {
    override def toString: String = "off"
  }

  /** Log level corresponding to severe errors, should be understandable to the
    * end-user.
    */
  case object Error extends LogLevel(0) {
    override def toString: String = "error"
  }

  /** Log level corresponding to important notices or issues that are not
    * severe.
    */
  case object Warning extends LogLevel(1) {
    override def toString: String = "warning"
  }

  /** Log level corresponding to usual information of what the application is
    * doing.
    */
  case object Info extends LogLevel(2) {
    override def toString: String = "info"
  }

  /** Log level used for debugging the application.
    *
    * The messages can be more complex and targeted at developers diagnosing the
    * application.
    */
  case object Debug extends LogLevel(3) {
    override def toString: String = "debug"
  }

  /** Log level used for advanced debugging, may be used for more throughout
    * diagnostics.
    */
  case object Trace extends LogLevel(4) {
    override def toString: String = "trace"
  }

  /** Lists all available log levels.
    *
    * Can be used for example to automate parsing.
    */
  val allLevels = Seq(
    LogLevel.Off,
    LogLevel.Error,
    LogLevel.Warning,
    LogLevel.Info,
    LogLevel.Debug,
    LogLevel.Trace
  )

  /** [[Ordering]] instance for [[LogLevel]].
    *
    * The log levels are ordered from most severe. If a log level is enabled, it
    * usually means that all levels smaller than it are enabled too.
    */
  implicit val ord: Ordering[LogLevel] = (x, y) => x.level - y.level

  /** [[Encoder]] instance for [[LogLevel]].
    */
  implicit val encoder: Encoder[LogLevel] = {
    case Off =>
      throw new IllegalArgumentException(
        "`None` log level should never be used in actual log messages and it " +
        "cannot be serialized to prevent that."
      )
    case level =>
      level.level.asJson
  }

  /** Creates a [[LogLevel]] from its integer representation.
    *
    * Returns None if the number does not represent a valid log level.
    */
  def fromInteger(level: Int): Option[LogLevel] = level match {
    case Error.level   => Some(Error)
    case Warning.level => Some(Warning)
    case Info.level    => Some(Info)
    case Debug.level   => Some(Debug)
    case Trace.level   => Some(Trace)
    case _             => None
  }

  /** [[Decoder]] instance for [[LogLevel]].
    */
  implicit val decoder: Decoder[LogLevel] = { json =>
    json.as[Int].flatMap { level =>
      fromInteger(level).toRight(
        DecodingFailure(s"`$level` is not a valid log level.", json.history)
      )
    }
  }

  /** Converts our internal [[LogLevel]] to the corresponding instance of
    * Akka-specific log level.
    */
  def toAkka(logLevel: LogLevel): akka.event.Logging.LogLevel = logLevel match {
    case Off     => akka.event.Logging.LogLevel(Int.MinValue)
    case Error   => akka.event.Logging.ErrorLevel
    case Warning => akka.event.Logging.WarningLevel
    case Info    => akka.event.Logging.InfoLevel
    case Debug   => akka.event.Logging.DebugLevel
    case Trace   => akka.event.Logging.DebugLevel
  }
}
