package org.enso.launcher.cli

import java.nio.file.Path

import org.enso.cli.CLIOutput
import org.enso.launcher.Logger
import org.enso.launcher.http.{HTTPDownload, HTTPRequestBuilder}
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
    HTTPDownload
      .download(
        HTTPRequestBuilder
          .fromURIString(
            "https://github.com/enso-org/enso-staging/releases/download/enso-0.1.1-rc3/enso-engine-0.1.1-rc3-linux-amd64.tar.gz"
          )
          .GET,
        Path.of("./lin.tar.gz")
      )
      .waitForResult(showProgress = true)
//    val r = HTTPDownload
//      .fetchString(
//        HTTPRequestBuilder
//          .fromURIString(
//            "https://github.com/enso-org/enso-staging/releases/download/enso-0.1.1-rc3/enso-launcher-0.1.1-rc3-linux-amd64.tar.gz"
//          )
//          .GET
//      )
//      .waitForResult(showProgress = true)
//    println(r)
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
