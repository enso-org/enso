package org.enso.loggingservice

import org.enso.logger.masking.Masking
import org.slf4j.{ILoggerFactory, Logger => SLF4JLogger}

/** A [[ILoggerFactory]] instance for the SLF4J backend. */
class LoggerFactory extends ILoggerFactory {

  private val loggers = scala.collection.concurrent.TrieMap[String, Logger]()

  /** @inheritdoc */
  override def getLogger(name: String): SLF4JLogger = {
    val logger = loggers.getOrElseUpdate(
      name,
      new Logger(name, LoggingServiceManager.Connection, Masking())
    )
    if (!Masking.isMaskingEnabled) {
      logger.warn("Log masking is disabled!")
    }
    logger
  }
}
