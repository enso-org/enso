package org.enso.logger

import com.typesafe.scalalogging.Logger

object LoggerSyntax {
  implicit class LoggerSyntax(logger: Logger) {
    def enter(contextName: String): Logger = {
      val name = logger.underlying.getName + "." + contextName
      Logger(name)
    }
  }
}
