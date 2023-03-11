package sbt.internal.util

import sbt.internal.LogManager
import sbt.internal.util.ConsoleAppender.{noSuppressedMessage, Properties}

object CustomLogManager {
  def excludeMsg(msgPrefix: String, level: sbt.Level.Value): LogManager = {
    sbt.internal.LogManager.withLoggers((_, _) =>
      new CustomAppender(level, msgPrefix, ConsoleOut.systemOut)
    )
  }

  /** Returns a custom ConsoleAppender that will skip log messages starting with a certain prefix.
    *
    * The only reason for such appender is to force SBT to keep quiet about certain kind of messages
    * coming from the analyzing compiler (wrapper around java compiler) when it tries to match class files
    * to source files. There is absolutely no way to tell SBT that that some sources are being generated from
    * annotation processors and it will get them during the same compilation round. Nor can we easily
    * suppress such warning.
    *
    * @param excludeLevel level of log message to exclude (together with prefix)
    * @param prefix prefix of log message to exclude (together with log level)
    * @param out object representing console output
    */
  final private class CustomAppender(
    excludeLevel: sbt.Level.Value,
    prefix: String,
    out: ConsoleOut
  ) extends ConsoleAppender(
        "out",
        Properties
          .from(out, Terminal.isAnsiSupported, Terminal.isAnsiSupported),
        noSuppressedMessage
      ) {
    override def appendLog(level: sbt.Level.Value, message: => String): Unit = {
      if (excludeLevel != level || !message.startsWith(prefix)) {
        super.appendLog(level, message)
      }
    }
  }
}
