package org.enso.launcher.cli

import com.typesafe.scalalogging.Logger
import org.enso.cli.CLIOutput
import org.enso.launcher.distribution.DefaultManagers
import org.enso.launcher.upgrade.LauncherUpgrader

/** Defines the entry point for the launcher.
  */
object Main {

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

  /** Entry point of the application.
    */
  def main(args: Array[String]): Unit = {
    // Disable logging prior to parsing arguments (may generate additional and unnecessary logs)
    LauncherLogging.initLogger()
    val exitCode =
      try {
        LauncherUpgrader.recoverUpgradeRequiredErrors(args) {
          runAppHandlingParseErrors(args)
        }
      } catch {
        case e: Exception =>
          LauncherLogging.setupFallback()
          logger.error("A fatal error has occurred: {}", e.getMessage, e)
          1
      }

    exit(exitCode)
  }

  /** Exits the program in a safe way.
    *
    * This should be used after `sys.exit` to ensure that all services are
    * terminated gracefully and locks are released quickly (as the OS cleanup
    * may take a longer while). The only exception is for functions in the
    * [[InternalOpts]], because they may need to terminate the program as
    * quickly as possible.
    *
    * @param exitCode exit code to return
    */
  def exit(exitCode: Int): Nothing = {
    LauncherLogging.tearDown()
    DefaultManagers.defaultResourceManager.releaseMainLock()
    sys.exit(exitCode)
  }
}
