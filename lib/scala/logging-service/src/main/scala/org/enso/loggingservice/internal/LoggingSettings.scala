package org.enso.loggingservice.internal

import java.util.Properties

import com.typesafe.config.{Config, ConfigFactory}
import org.enso.loggingservice.LogLevel

import scala.collection.immutable.ListMap
import scala.util.Using

/** Reads logger settings from the resources.
  *
  * Currently these settings are used to configure logging inside of tests.
  */
object LoggingSettings {

  private object Key {
    val root   = "logging-service"
    val logger = "logger"
  }

  private val configuration: Config = {
    val empty = ConfigFactory.empty().atKey(Key.logger).atKey(Key.root)
    ConfigFactory.load().withFallback(empty).getConfig(Key.root)
  }

  /** Log level settings overriding the default application log level.
    *
    * @return a mapping from a logger name to the log level that will be used
    * for that logger.
    */
  val loggers: Map[String, LogLevel] = {
    def normalize(key: String): String =
      key.replace("'", "").replace("\"", "")

    val builder      = ListMap.newBuilder[String, LogLevel]
    val loggerConfig = configuration.getConfig(Key.logger)

    loggerConfig.entrySet.forEach { entry =>
      val key   = entry.getKey
      val value = loggerConfig.getString(key)
      LogLevel.fromString(value) match {
        case Some(logLevel) =>
          builder += normalize(key) -> logLevel
        case None =>
          System.err.println(
            s"Invalid log level for key [${normalize(key)}] set in " +
            s"application config [$value]. Default log level will be used."
          )
      }
    }

    builder.result()
  }

  private val propertiesFilename     = "logging.properties"
  private val testLoggingPropertyKey = "test-log-level"
  private val loggingProperties: Properties = {
    val props = new Properties
    Option(this.getClass.getClassLoader.getResourceAsStream(propertiesFilename))
      .foreach { stream =>
        val _ = Using(stream) { stream =>
          props.load(stream)
        }
      }
    props
  }

  /** Indicates the log level to be used in test mode.
    *
    * If set to None, production logging should be used.
    */
  val testLogLevel: Option[LogLevel] = Option(
    loggingProperties.getProperty(testLoggingPropertyKey)
  ).map { string =>
    LogLevel.fromString(string).getOrElse {
      System.err.println(
        s"Invalid log level for $testLoggingPropertyKey set in " +
        s"$propertiesFilename, falling back to info."
      )
      LogLevel.Info
    }
  }

}
