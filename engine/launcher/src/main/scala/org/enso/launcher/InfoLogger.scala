package org.enso.launcher

import com.typesafe.scalalogging.Logger
import org.enso.cli.CLIOutput

/**
  * Handles displaying of user-facing information.
  *
  * Info-level messages are used to communicate with the user. They are handled
  * in a special way, so that they are displayed to the user regardless of
  * logging settings.
  */
object InfoLogger {

  private val logger = Logger("launcher")

  /**
    * Prints an info level message.
    *
    * If the default logger is set-up to display info-messages, they are send to
    * the logger, otherwise they are printed to stdout.
    *
    * It is important to note that these messages should always be displayed to
    * the user, so unless run in debug mode, all launcher settings should ensure
    * that info-level logs are printed to the console output.
    */
  def info(msg: => String): Unit = {
    if (logger.underlying.isInfoEnabled) {
      logger.info(msg)
    } else {
      CLIOutput.println(msg)
    }
  }

}
