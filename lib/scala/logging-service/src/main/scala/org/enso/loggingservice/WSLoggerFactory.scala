package org.enso.loggingservice

import org.slf4j.{ILoggerFactory, Logger}

class WSLoggerFactory extends ILoggerFactory {
  override def getLogger(name: String): Logger =
    new WSLogger(name, WSLoggerManager.Connection)
}
