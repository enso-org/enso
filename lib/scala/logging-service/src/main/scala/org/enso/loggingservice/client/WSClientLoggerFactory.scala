package org.enso.loggingservice.client

import org.enso.loggingservice.WSLogger
import org.slf4j.{ILoggerFactory, Logger}

class WSClientLoggerFactory extends ILoggerFactory {
  override def getLogger(name: String): Logger =
    new WSLogger(name, WSConnectionManager.Connection)
}
