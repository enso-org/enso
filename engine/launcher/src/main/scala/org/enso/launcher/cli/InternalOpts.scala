package org.enso.launcher.cli

import java.io.IOException
import java.nio.file.{Files, NoSuchFileException, Path}

import org.enso.cli.Opts
import org.enso.cli.Opts.implicits._
import cats.implicits._
import org.enso.launcher.{FileSystem, OS}
import org.enso.launcher.FileSystem.PathSyntax

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
  * 2. Finish Uninstall
  *    After uninstalling the distribution, we need to remove the executable,
  *    but for the same reasons as above, we need a workaround. The uninstaller
  *    creates a copy of itself in a temporary directory (which will be removed
  *    when the system removes temporary files) and uses it to remove the
  *    original binary. It then can also remove the (now empty) installation
  *    directory if necessary.
  */
object InternalOpts {
  private val REMOVE_OLD_EXECUTABLE   = "internal-remove-old-executable"
  private val FINISH_UNINSTALL        = "internal-finish-uninstall"
  private val FINISH_UNINSTALL_PARENT = "internal-finish-uninstall-parent"

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

    val finishUninstallOpt = Opts
      .optionalParameter[Path](
        FINISH_UNINSTALL,
        "PATH",
        "Removes the old executable."
      )
      .hidden
    val finishUninstallParentOpt = Opts
      .optionalParameter[Path](
        FINISH_UNINSTALL_PARENT,
        "PATH",
        s"Removes the possible parent directories of the old executable. " +
        s"To be used only in conjunction with $FINISH_UNINSTALL. Has no " +
        s"effect if used without that option."
      )
      .hidden

    (
      removeOldExecutableOpt,
      finishUninstallOpt,
      finishUninstallParentOpt
    ) mapN {
      (removeOldExecutableOpt, finishUninstallOpt, finishUninstallParentOpt) =>
        removeOldExecutableOpt.foreach { oldExecutablePath =>
          removeOldExecutable(oldExecutablePath)
          sys.exit(0)
        }

        finishUninstallOpt.foreach { executablePath =>
          finishUninstall(executablePath, finishUninstallParentOpt)
          sys.exit(0)
        }
    }
  }

  /**
    * Returns a helper class that allows to run the launcher located at the
    * provided path invoking the internal options.
    */
  def runWithNewLauncher(pathToNewLauncher: Path): Runner =
    new Runner(pathToNewLauncher)

  /**
    * A helper class used for running the workarounds using another launcher
    * executable.
    */
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

    /**
      * Tells the temporary launcher to remove the original launcher executable
      * and possibly its parent directory.
      *
      * The parent directory is removed if it is empty or only contains an empty
      * `bin` directory. This is used for cases when the executable is inside of
      * the data directory and the data directory cannot be fully removed before
      * the executable has been removed.
      *
      * @param executablePath path to the old executable
      * @param parentToRemove path to the parent directory
      */
    def finishUninstall(
      executablePath: Path,
      parentToRemove: Option[Path]
    ): Nothing = {
      val parentParam = parentToRemove.map(parent =>
        Seq(s"--$FINISH_UNINSTALL_PARENT", parent.toAbsolutePath.toString)
      )
      val command = Seq(
          pathToNewLauncher.toAbsolutePath.toString,
          s"--$FINISH_UNINSTALL",
          executablePath.toAbsolutePath.toString
        ) ++ parentParam.getOrElse(Seq())
      runDetachedAndExit(command)
    }
  }

  private val retryBaseAmount = 30

  /**
    * Tries to remove the file at `oldExecutablePath`, retrying several times if
    * needed.
    *
    * On failure retries every 0.5s for 15s in total. That retry mechanism is in
    * place, because a running executable cannot be removed and it may take some
    * time for the process to fully terminate (theoretically this time can be
    * extended indefinitely, for example if anti-virus software blocks the
    * executable for scanning, so this may still fail).
    */
  @scala.annotation.tailrec
  private def tryDeleting(oldExecutablePath: Path, retries: Int = 30): Unit = {
    try {
      Files.delete(oldExecutablePath)
    } catch {
      case _: NoSuchFileException =>
      case e: IOException =>
        def firstTime = retries == retryBaseAmount
        if (firstTime) {
          System.err.println(
            s"Could not remove $oldExecutablePath, will retry several " +
            s"times for 15s..."
          )
        }

        if (retries > 0) {
          Thread.sleep(500)
          tryDeleting(oldExecutablePath, retries - 1)
        } else {
          e.printStackTrace()
          System.err.println(
            s"Cannot delete old executable at $oldExecutablePath after " +
            s"multiple retries. Please try removing it manually."
          )
        }
    }
  }

  private def removeOldExecutable(oldExecutablePath: Path): Unit =
    tryDeleting(oldExecutablePath)

  private def finishUninstall(
    executablePath: Path,
    parentToRemove: Option[Path]
  ): Unit = {
    tryDeleting(executablePath)
    parentToRemove.foreach { parent =>
      val bin = parent / "bin"
      if (Files.exists(bin))
        FileSystem.removeDirectoryIfEmpty(bin)
      FileSystem.removeDirectoryIfEmpty(parent)
    }
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
