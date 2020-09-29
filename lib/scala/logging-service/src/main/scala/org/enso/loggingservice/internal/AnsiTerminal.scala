package org.enso.loggingservice.internal

import com.typesafe.scalalogging.Logger
import org.graalvm.nativeimage.ImageInfo

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
}
