package org.enso.runtimeversionmanager.distribution

import java.nio.file.{Files, Path}
import com.typesafe.scalalogging.Logger
import org.enso.distribution.{DistributionManager, FileSystem}
import org.enso.distribution.locking.ResourceManager

/** Manages safe access to the temporary directory.
  *
  * The temporary directory is created on demand and automatically removed if it
  * is empty. Temporary files from previous runs are removed when the temporary
  * directory is first accessed. Locking mechanism is used to ensure that the
  * old files are no longer used by any other instances running in parallel.
  */
class TemporaryDirectoryManager(
  distribution: DistributionManager,
  resourceManager: ResourceManager
) {
  private val logger = Logger[TemporaryDirectoryManager]

  /** Returns path to a directory for storing temporary files that is located on
    * the same filesystem as `runtimes` and `engines`.
    *
    * It is used during installation to decrease the possibility of getting a
    * broken installation if the installation process has been abruptly
    * terminated. The directory is created on demand (when its path is requested
    * for the first time) and is removed if the application exits normally (as
    * long as it is empty, but normal termination of the installation process
    * should ensure that). If that fails, it is also cleaned before any future
    * accesses.
    */
  def accessTemporaryDirectory(): Path = safeTemporaryDirectory

  private lazy val safeTemporaryDirectory = {
    resourceManager.startUsingTemporaryDirectory()
    distribution.paths.unsafeTemporaryDirectory
  }

  /** Tries to clean the temporary files directory.
    *
    * It should be run at startup whenever the program wants to run clean-up.
    * Currently it is run when installation-related operations are taking place.
    * It may not proceed if another process is using it. It has to be run before
    * the first access to the temporaryDirectory, as after that the directory is
    * marked as in-use and will not be cleaned.
    */
  def tryCleaningTemporaryDirectory(): Unit = {
    val tmp = distribution.paths.unsafeTemporaryDirectory
    if (Files.exists(tmp)) {
      resourceManager.tryWithExclusiveTemporaryDirectory {
        if (!FileSystem.isDirectoryEmpty(tmp)) {
          logger.info(
            "Cleaning up temporary files from a previous installation."
          )
        }
        FileSystem.removeDirectory(tmp)
        Files.createDirectories(tmp)
        FileSystem.removeEmptyDirectoryOnExit(tmp)
      }
    }
  }
}
