package org.enso.loggingservice.internal

import org.enso.loggingservice.LogLevel

/** An interface that allows to send log messages to the logging service. */
trait LoggerConnection {

  /** Sends a message to the logging service.
    *
    * It should return immediately. Sending a message usually means that it is
    * enqueued and will be encoded and sent to the logging service soon, but it
    * is possible for messages to be dropped if too many messages are logged in
    * a short period of time.
    */
  def send(message: InternalLogMessage): Unit

  /** Current log level.
    *
    * Only messages that have equal or smaller log level should be sent. Other
    * messages will be ignored.
    */
  def logLevel: LogLevel

  def loggerConfigs: Seq[LoggerConfig]

  /** Tells if messages with the provided log level should be sent. */
  def isEnabled(name: String, level: LogLevel): Boolean = {
    val groupLevel =
      loggerConfigs
        .find(config => name.startsWith(config.name))
        .map(_.level)
        .getOrElse(logLevel)
    implicitly[Ordering[LogLevel]].lteq(level, groupLevel)
  }
}
