package org.enso.launcher

import java.io.IOError
import java.nio.file.{Files, NoSuchFileException, Path}

import org.enso.cli.Opts
import org.enso.cli.Opts.implicits._
import cats.implicits._
import org.enso.launcher.internal.OS
import sys.process._

object InternalOpts {
  private def REMOVE_OLD_EXECUTABLE = "internal-remove-old-executable"

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

  def runWithNewLauncher(pathToNewLauncher: Path): Runner =
    new Runner(pathToNewLauncher)

  class Runner private[InternalOpts] (pathToNewLauncher: Path) {
    def removeOldExecutableAndExit(oldExecutablePath: Path): Nothing = {
      val command = Seq(
        pathToNewLauncher.toAbsolutePath.toString,
        REMOVE_OLD_EXECUTABLE,
        oldExecutablePath.toAbsolutePath.toString
      )
      runDetachedAndExit(command)
    }
  }

  private def removeOldExecutable(oldExecutablePath: Path): Unit = {
    @scala.annotation.tailrec
    def tryDeleting(retries: Int): Unit = {
      try {
        Files.delete(oldExecutablePath)
      } catch {
        case _: NoSuchFileException =>
        case e: IOError =>
          if (retries > 0) {
            Thread.sleep(500)
            tryDeleting(retries - 1)
          } else {
            println(
              s"Cannot delete old executable at $oldExecutablePath after " +
              s"multiple retries. Please try removing it manually."
            )
            e.printStackTrace()
          }
      }
    }

    tryDeleting(10)
  }

  private def runDetachedAndExit(command: Seq[String]): Nothing = {
    if (!OS.isWindows) {
      throw new IllegalStateException(
        "Internal error: Detached process workarounds are only available on " +
        "Windows."
      )
    }

    command.run()
    sys.exit()
  }
}
