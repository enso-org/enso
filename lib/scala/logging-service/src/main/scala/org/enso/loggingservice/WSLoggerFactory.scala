package org.enso.loggingservice

import org.slf4j.{ILoggerFactory, Logger}

class WSLoggerFactory extends ILoggerFactory {
  override def getLogger(name: String): Logger = {
    loggers.getOrElseUpdate(
      name,
      new WSLogger(name, WSLoggerManager.Connection)
    )
  }

  private val loggers = scala.collection.concurrent.TrieMap[String, Logger]()
}
