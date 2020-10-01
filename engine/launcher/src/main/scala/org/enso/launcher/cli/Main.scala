package org.enso.launcher.cli

import com.typesafe.scalalogging.Logger
import org.enso.cli.CLIOutput
import org.enso.launcher.locking.DefaultResourceManager
import org.enso.launcher.upgrade.LauncherUpgrader
import org.enso.loggingservice.LoggingServiceManager

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

  /**
    * Entry point of the application.
    */
  def main(args: Array[String]): Unit = {
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

  /**
    * Exits the program in a safe way.
    *
    * It is not strictly necessary to favour this method over `sys.exit`, as all
    * resources should be released anyway, but using it makes sure that they are
    * released immediately whereas with `sys.exit`, it is the obligation of the
    * OS to release them and it such cleanup (especially releasing locks) may
    * take more time. It also ensures that the logging service is properly
    * terminated and all logfiles are flushed.
    *
    * @param exitCode exit code to return
    */
  def exit(exitCode: Int): Nothing = {
    LoggingServiceManager.tearDown()
    DefaultResourceManager.releaseMainLock()
    System.err.println("Bye bye")
    (new RuntimeException("Bye bye")).fillInStackTrace().printStackTrace()
    Thread.sleep(100)
    System.out.flush()
    System.err.flush()
    sys.exit(exitCode)
  }
}
