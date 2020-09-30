package org.enso

import com.typesafe.scalalogging.Logger

package object logger {

  /**
    * Provides syntax for entering a sub-logger.
    */
  implicit class LoggerSyntax(logger: Logger) {

    /**
      * Returns another [[Logger]] with name extended with a sub context.
      */
    def enter(subContextName: String): Logger = {
      val name = logger.underlying.getName + "." + subContextName
      Logger(name)
    }
  }
}
