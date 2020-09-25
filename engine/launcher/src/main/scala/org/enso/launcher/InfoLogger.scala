package org.enso.launcher

import com.typesafe.scalalogging.Logger
import org.enso.cli.CLIOutput

/**
  * Handles displaying of user-facing information.
  *
  * Info-level messages are used to communicate with the user. This class
  * handles them in special way, so that they are displayed to the user
  * regardless of logging settings.
  */
object InfoLogger {

  private val logger = Logger("launcher")

  /**
    * Prints an info level message.
    *
    * If the default logger is set-up to display info-messages, they are send to
    * the logger, otherwise they are printed to stdout.
    */
  def info(msg: => String): Unit = {
    if (logger.underlying.isInfoEnabled) {
      logger.info(msg)
    } else {
      CLIOutput.println(msg)
    }
  }

}
