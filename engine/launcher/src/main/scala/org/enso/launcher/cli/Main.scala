package org.enso.launcher.cli

import com.typesafe.scalalogging.Logger
import org.enso.cli.CLIOutput
import org.enso.launcher.locking.DefaultResourceManager
import org.enso.launcher.upgrade.LauncherUpgrader
import org.enso.loggingservice.WSLoggerManager
import org.enso.nativeimage.workarounds.AnsiTerm

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
        LauncherLogging.setupFallback()
        CLIOutput.println(errors.mkString("\n"))
        1
      case Right(exitCode) =>
        exitCode
    }

  private val logger = Logger[Main.type]

  def main(args: Array[String]): Unit = {
    logger.debug("DEBUG BEFORE SETUP")
    val handle = AnsiTerm.GetStdHandle(AnsiTerm.STD_ERROR_HANDLE)
    AnsiTerm.SetConsoleMode(handle, 0)
    setup()
    val exitCode =
      try {
        LauncherUpgrader.recoverUpgradeRequiredErrors(args) {
          runAppHandlingParseErrors(args)
        }
      } catch {
        case e: Exception =>
          logger.error(s"A fatal error has occurred: $e", e)
          1
      }

    exit(exitCode)
  }

  def exit(exitCode: Int): Nothing = {
    WSLoggerManager.tearDown()
    DefaultResourceManager.releaseMainLock()
    sys.exit(exitCode)
  }
}
