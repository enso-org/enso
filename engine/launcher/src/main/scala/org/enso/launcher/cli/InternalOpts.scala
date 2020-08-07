package org.enso.launcher.cli

import java.io.IOException
import java.nio.file.{Files, NoSuchFileException, Path}

import org.enso.cli.Opts
import org.enso.cli.Opts.implicits._
import cats.implicits._
import org.enso.launcher.OS

/**
  * Implements internal options that the launcher may use when running another
  * instance of itself.
  *
  * These options are used primarily to implement workarounds for
  * Windows-specific filesystem limitations. They should not be used by the
  * users directly, so they are not displayed in the help text.
  *
  * The implemented workarounds are following:
  *
  * 1. Remove Old Executable
  *    On Windows, if an executable is running, its file is locked, so it is
  *    impossible to remove a file that is running. Thus it is not possible for
  *    the launcher to directly remove its old executable when it finishes the
  *    installation. Instead it launches the newly installed launcher, which
  *    tries to remove it, and terminates itself. Thus the executable lock is
  *    soon freed and the new launcher is able to remove the old one. The new
  *    launcher attempts several retries, because it may take some time for the
  *    executable to be unlocked (especially as on Windows software like an
  *    antivirus may block it for some more time after terminating).
  */
object InternalOpts {
  private val REMOVE_OLD_EXECUTABLE = "internal-remove-old-executable"

  /**
    * Additional top level options that are internal to the launcher and should
    * not be used by users directly.
    *
    * They are used to implement workarounds for install / upgrade on Windows.
    */
  def topLevelOptions: Opts[Unit] = {
    val removeOldExecutableOpt = Opts
      .optionalParameter[Path](
        REMOVE_OLD_EXECUTABLE,
        "PATH",
        "Removes the old executable file after the installation. " +
        "Used as a workaround for file locking on Windows."
      )
      .hidden

    removeOldExecutableOpt map {
      case Some(oldExecutablePath) =>
        removeOldExecutable(oldExecutablePath)
        sys.exit(0)
      case None =>
    }
  }

  /**
    * Returns a helper class that allows to run the launcher located at the
    * provided path invoking the internal options.
    */
  def runWithNewLauncher(pathToNewLauncher: Path): Runner =
    new Runner(pathToNewLauncher)

  class Runner private[InternalOpts] (pathToNewLauncher: Path) {

    /**
      * Tells the installed launcher to try to remove the old launcher
      * executable.
      *
      * It retries for a few seconds to give the process running the old
      * launcher to terminate and release the lock on its file.
      */
    def removeOldExecutableAndExit(oldExecutablePath: Path): Nothing = {
      val command = Seq(
        pathToNewLauncher.toAbsolutePath.toString,
        s"--$REMOVE_OLD_EXECUTABLE",
        oldExecutablePath.toAbsolutePath.toString
      )
      runDetachedAndExit(command)
    }
  }

  private def removeOldExecutable(oldExecutablePath: Path): Unit = {
    val retryBaseAmount = 30
    @scala.annotation.tailrec
    def tryDeleting(retries: Int): Unit = {
      try {
        Files.delete(oldExecutablePath)
      } catch {
        case _: NoSuchFileException =>
        case e: IOException =>
          if (retries == retryBaseAmount) {
            System.err.println(
              s"Could not remove $oldExecutablePath, will retry several " +
              s"times for 15s..."
            )
          }

          if (retries > 0) {
            Thread.sleep(500)
            tryDeleting(retries - 1)
          } else {
            e.printStackTrace()
            System.err.println(
              s"Cannot delete old executable at $oldExecutablePath after " +
              s"multiple retries. Please try removing it manually."
            )
          }
      }
    }

    tryDeleting(retryBaseAmount)
  }

  private def runDetachedAndExit(command: Seq[String]): Nothing = {
    if (!OS.isWindows) {
      throw new IllegalStateException(
        "Internal error: Detached process workarounds are only available on " +
        "Windows."
      )
    }

    val pb = new java.lang.ProcessBuilder(command: _*)
    pb.redirectOutput(java.lang.ProcessBuilder.Redirect.INHERIT)
    pb.start()
    sys.exit()
  }
}
