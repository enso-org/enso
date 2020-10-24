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

  /** [[Decoder]] instance for [[LogLevel]].
    */
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
