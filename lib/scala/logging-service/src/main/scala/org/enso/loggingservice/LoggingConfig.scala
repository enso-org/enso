package org.enso.loggingservice

import java.nio.file.Path

case class LoggingConfig(
  stderrEnabled: Boolean,
  ansiColors: Boolean,
  outputToFile: Option[Path],
  bufferDelaySeconds: Int = 3
)

object LoggingConfig {
  object Default
      extends LoggingConfig(
        stderrEnabled = true,
        ansiColors    = false,
        outputToFile  = None
      )
}
