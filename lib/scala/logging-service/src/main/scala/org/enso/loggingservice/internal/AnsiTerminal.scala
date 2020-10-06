package org.enso.loggingservice.internal

import com.typesafe.scalalogging.Logger
import org.graalvm.nativeimage.ImageInfo

/**
  * Handles VT-compatible color output in the terminal.
  */
object AnsiTerminal {

  /**
    * Tries enabling ANSI colors in terminal output and returns true if it
    * succeeded.
    *
    * We assume that ANSI colors are supported by default on UNIX platforms. On
    * Windows, we use native calls to enable them, currently this is only
    * supported in native-image builds. Currently ANSI colors are not supported
    * on non-native Windows targets.
    */
  def tryEnabling(): Boolean = {
    if (isWindows) {
      if (ImageInfo.inImageCode) {
        try {
          NativeAnsiTerm.enableVT()
          true
        } catch {
          case error: RuntimeException =>
            Logger[AnsiTerminal.type].warn(
              s"Failed to initialize VT terminal (output will not contain " +
              s"colors): ${error.getMessage}"
            )
            false
        }
      } else false
    } else true
  }

  private def isWindows: Boolean =
    System.getProperty("os.name").toLowerCase.contains("win")

  /**
    * Checks if output of this program may be piped.
    */
  def isLikelyPiped: Boolean = System.console() == null

  /**
    * Checks if the output is connected to a terminal that can handle color
    * output.
    *
    * On Windows, this function also enables color output, so any code that
    * wants to use VT escape codes for colors (and is not assuming that its
    * output is redirected) should first call this function to try enabling it
    * and only use them if this function returned true.
    */
  def canUseColors(): Boolean = {
    !isLikelyPiped && AnsiTerminal.tryEnabling()
  }
}
