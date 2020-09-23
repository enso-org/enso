package org.enso

import com.typesafe.scalalogging.Logger

package object logger {
  implicit class LoggerSyntax(logger: Logger) {
    def enter(contextName: String): Logger = {
      val name = logger.underlying.getName + "." + contextName
      Logger(name)
    }
  }
}
