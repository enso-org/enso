package org.enso.loggingservice.internal

import java.util.Properties

import org.enso.loggingservice.LogLevel

import scala.util.Using

/** Reads logger settings from the resources.
  *
  * Currently these settings are used to configure logging inside of tests.
  */
object LoggingSettings {
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
