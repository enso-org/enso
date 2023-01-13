package org.enso.loggingservice.internal

import com.typesafe.config.{Config, ConfigFactory}
import org.enso.loggingservice.LogLevel

import scala.collection.immutable.ListMap

/** Reads logger settings from the resources.
  *
  * Currently these settings are used to configure logging inside of tests.
  */
object LoggingSettings {

  private object Key {
    val root         = "logging-service"
    val logger       = "logger"
    val testLogLevel = "test-log-level"
    val GLOB         = "*"
  }

  private lazy val configuration: Config = {
    val empty = ConfigFactory.empty().atKey(Key.logger).atKey(Key.root)
    ConfigFactory.load().withFallback(empty).getConfig(Key.root)
  }

  /** Log level settings overriding the default application log level.
    *
    * @return a mapping from a logger name to the log level that will be used
    * for that logger.
    */
  lazy val loggers: Map[String, LogLevel] = {
    def normalize(key: String): String =
      key.replace("'", "").replace("\"", "")

    val loggerConfig = configuration.getConfig(Key.logger)
    val builder      = ListMap.newBuilder[String, LogLevel]
    // `config` is unordered. To keep glob (*) entries at the end of the `ListMap`,
    // gather them at the `fallback` map, and then append to the final `builder`
    val fallback = ListMap.newBuilder[String, LogLevel]

    loggerConfig.entrySet.forEach { entry =>
      val key   = entry.getKey
      val value = loggerConfig.getString(key)
      LogLevel.fromString(value) match {
        case Some(logLevel) =>
          val normalizedKey = normalize(key)
          if (normalizedKey.endsWith(Key.GLOB)) {
            fallback += normalizedKey.dropRight(Key.GLOB.length + 1) -> logLevel
          } else {
            builder += normalizedKey -> logLevel
          }
        case None =>
          System.err.println(
            s"Invalid log level for key [${normalize(key)}] set in " +
            s"application config [$value]. Default log level will be used."
          )
      }
    }

    builder ++= fallback.result()
    builder.result()
  }

  /** Indicates the log level to be used in test mode.
    *
    * If set to None, production logging should be used.
    */
  lazy val testLogLevel: Option[LogLevel] = {
    Option.when(configuration.hasPath(Key.testLogLevel)) {
      val value = configuration.getString(Key.testLogLevel)
      LogLevel.fromString(value).getOrElse {
        System.err.println(
          s"Invalid log level for key [${Key.testLogLevel}] set in " +
          s"application config [$value], falling back to info."
        )
        LogLevel.Info
      }
    }
  }

}
