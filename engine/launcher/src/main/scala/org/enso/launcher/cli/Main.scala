package org.enso.launcher.cli

import org.enso.cli.CLIOutput
import org.enso.launcher.Logger
import org.enso.launcher.locking.DefaultResourceManager
import org.enso.launcher.upgrade.LauncherUpgrader

/**
  * Defines the entry point for the launcher.
  */
object Main {
  private def setup(): Unit =
    System.setProperty(
      "org.apache.commons.logging.Log",
      "org.apache.commons.logging.impl.NoOpLog"
    )

  private def runAppHandlingParseErrors(args: Array[String]): Int =
    LauncherApplication.application.run(args) match {
      case Left(errors) =>
        CLIOutput.println(errors.mkString("\n"))
        1
      case Right(exitCode) =>
        exitCode
    }

  def main(args: Array[String]): Unit = {
    setup()
    val exitCode =
      try {
        LauncherUpgrader.recoverUpgradeRequiredErrors(args) {
          runAppHandlingParseErrors(args)
        }
      } catch {
        case e: Exception =>
          Logger.error(s"A fatal error has occurred: $e", e)
          1
      }

    DefaultResourceManager.releaseMainLock()
    sys.exit(exitCode)
  }
}
