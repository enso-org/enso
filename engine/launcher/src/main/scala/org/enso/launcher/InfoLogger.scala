package org.enso.launcher

import org.enso.cli.CLIOutput

/** Handles displaying of user-facing information.
  *
  * Info-level messages are used to communicate with the user. They are handled
  * in a special way, so that they are displayed to the user regardless of
  * logging settings.
  */
object InfoLogger {

  /** Prints an info level message.
    *
    * Currently, the message is always printed to standard output. But this may be changed by changing this method.
    */
  def info(msg: => String): Unit = {
    CLIOutput.println(msg)
  }

}
