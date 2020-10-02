package org.enso.loggingservice

import org.slf4j.{ILoggerFactory, Logger => SLF4JLogger}

/**
  * A [[ILoggerFactory]] instance for the SLF4J backend.
  */
class LoggerFactory extends ILoggerFactory {

  /**
    * @inheritdoc
    */
  override def getLogger(name: String): SLF4JLogger = {
    loggers.getOrElseUpdate(
      name,
      new Logger(name, LoggingServiceManager.Connection)
    )
  }

  private val loggers = scala.collection.concurrent.TrieMap[String, Logger]()
}
