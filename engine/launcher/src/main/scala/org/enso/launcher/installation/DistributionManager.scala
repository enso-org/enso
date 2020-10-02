package org.enso.launcher.installation

import java.nio.file.{Files, Path}

import com.typesafe.scalalogging.Logger
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.locking.{DefaultResourceManager, ResourceManager}
import org.enso.launcher.{Environment, FileSystem, InfoLogger, OS}

import scala.util.Try
import scala.util.control.NonFatal

/**
  * Gathers filesystem paths used by the launcher.
  *
  * @param dataRoot the root of the data directory; for a portable distribution
  *                 this is the root of the distribution, for a locally
  *                 installed distribution, it corresponds to `ENSO_DATA_DIR`
  * @param runtimes location of runtimes, corresponding to `runtime` directory
  * @param engines location of engine versions, corresponding to `dist`
  *                directory
  * @param config location of configuration
  * @param locks a directory for storing lockfiles that are used to synchronize
  *              access to the various components
  * @param logs a directory for storing logs
  * @param tmp a directory for storing temporary files that is located on the
  *            same filesystem as `runtimes` and `engines`, used during
  *            installation to decrease the possibility of getting a broken
  *            installation if the installation process has been abruptly
  *            terminated. The directory is created on demand (when its path is
  *            requested for the first time) and is removed if the application
  *            exits normally (as long as it is empty, but normal termination of
  *            the installation process should ensure that).
  * @param resourceManager reference to the resource manager used for
  *                        synchronizing access to the temporary files
  */
case class DistributionPaths(
  dataRoot: Path,
  runtimes: Path,
  engines: Path,
  config: Path,
  locks: Path,
  logs: Path,
  tmp: Path,
  resourceManager: ResourceManager
) {

  /**
    * @inheritdoc
    */
  override def toString: String =
    s"""DistributionPaths(
       |  dataRoot = $dataRoot,
       |  runtimes = $runtimes,
       |  engines  = $engines,
       |  config   = $config,
       |  locks    = $locks,
       |  tmp      = $tmp
       |)""".stripMargin

  lazy val temporaryDirectory: Path = {
    resourceManager.startUsingTemporaryDirectory()
    tmp
  }
}

/**
  * A helper class that detects if a portable or installed distribution is run
  * and encapsulates management of paths to components of the distribution.
  */
class DistributionManager(
  val env: Environment,
  resourceManager: ResourceManager
) {
  private val logger = Logger[DistributionManager]

  /**
    * Specifies whether the launcher has been run as a portable distribution or
    * it is a locally installed distribution.
    */
  lazy val isRunningPortable: Boolean = {
    val portable = detectPortable()
    logger.debug(s"Launcher portable mode = $portable")
    if (portable && LocallyInstalledDirectories.installedDistributionExists) {
      val installedRoot   = LocallyInstalledDirectories.dataDirectory
      val installedBinary = LocallyInstalledDirectories.binaryExecutable

      logger.debug(
        s"The launcher is run in portable mode, but an installed distribution" +
        s" is available at $installedRoot."
      )

      if (Files.exists(installedBinary)) {
        if (installedBinary == env.getPathToRunningExecutable) {
          logger.debug(
            "That distribution seems to be corresponding to this launcher " +
            "executable, that is running in portable mode."
          )
        } else {
          logger.debug(
            s"However, that installed distribution most likely uses another " +
            s"launcher executable, located at $installedBinary."
          )
        }
      }
    }
    portable
  }

  /**
    * Determines paths that should be used by the launcher.
    */
  lazy val paths: DistributionPaths = {
    val paths = detectPaths()
    logger.debug(s"Detected paths are: $paths")
    paths
  }

  private val PORTABLE_MARK_FILENAME = ".enso.portable"
  val ENGINES_DIRECTORY              = "dist"
  val RUNTIMES_DIRECTORY             = "runtime"
  val CONFIG_DIRECTORY               = "config"
  val BIN_DIRECTORY                  = "bin"
  val LOCK_DIRECTORY                 = "lock"
  val LOG_DIRECTORY                  = "log"
  val TMP_DIRECTORY                  = "tmp"

  private def detectPortable(): Boolean = Files.exists(portableMarkFilePath)
  private def possiblePortableRoot: Path =
    env.getPathToRunningExecutable.getParent.getParent

  private def portableMarkFilePath: Path =
    possiblePortableRoot / PORTABLE_MARK_FILENAME

  private def detectPaths(): DistributionPaths =
    if (isRunningPortable) {
      val root = env.getPathToRunningExecutable.getParent.getParent
      DistributionPaths(
        dataRoot = root,
        runtimes = root / RUNTIMES_DIRECTORY,
        engines  = root / ENGINES_DIRECTORY,
        config   = root / CONFIG_DIRECTORY,
        locks    = root / LOCK_DIRECTORY,
        logs     = root / LOG_DIRECTORY,
        tmp      = root / TMP_DIRECTORY,
        resourceManager
      )
    } else {
      val dataRoot   = LocallyInstalledDirectories.dataDirectory
      val configRoot = LocallyInstalledDirectories.configDirectory
      val runRoot    = LocallyInstalledDirectories.runtimeDirectory
      DistributionPaths(
        dataRoot = dataRoot,
        runtimes = dataRoot / RUNTIMES_DIRECTORY,
        engines  = dataRoot / ENGINES_DIRECTORY,
        config   = configRoot,
        locks    = runRoot / LOCK_DIRECTORY,
        logs     = LocallyInstalledDirectories.logDirectory,
        tmp      = dataRoot / TMP_DIRECTORY,
        resourceManager
      )
    }

  /**
    * Tries to clean the temporary files directory.
    *
    * It should be run at startup whenever the program wants to run clean-up.
    * Currently it is run when installation-related operations are taking place.
    * It may not proceed if another process is using it. It has to be run before
    * the first access to the temporaryDirectory, as after that the directory is
    * marked as in-use and will not be cleaned.
    */
  def tryCleaningTemporaryDirectory(): Unit = {
    val tmp = paths.tmp
    if (Files.exists(tmp)) {
      resourceManager.tryWithExclusiveTemporaryDirectory {
        if (!FileSystem.isDirectoryEmpty(tmp)) {
          InfoLogger.info(
            "Cleaning up temporary files from a previous installation."
          )
        }
        FileSystem.removeDirectory(tmp)
        Files.createDirectories(tmp)
        FileSystem.removeEmptyDirectoryOnExit(tmp)
      }
    }
  }

  /**
    * Removes unused lockfiles.
    */
  def tryCleaningUnusedLockfiles(): Unit = {
    val lockfiles = FileSystem.listDirectory(paths.locks)
    for (lockfile <- lockfiles) {
      try {
        Files.delete(lockfile)
        logger.debug(s"Removed unused lockfile ${lockfile.getFileName}.")
      } catch {
        case NonFatal(_) =>
      }
    }
  }

  /**
    * A helper for managing directories of the non-portable installation.
    *
    * It returns paths of the non-portable installation even if the launcher is
    * running in portable mode, so that this helper can be used by the installer
    * to determine destination for installed files.
    */
  object LocallyInstalledDirectories {
    val ENSO_DATA_DIRECTORY    = "ENSO_DATA_DIRECTORY"
    val ENSO_CONFIG_DIRECTORY  = "ENSO_CONFIG_DIRECTORY"
    val ENSO_BIN_DIRECTORY     = "ENSO_BIN_DIRECTORY"
    val ENSO_RUNTIME_DIRECTORY = "ENSO_RUNTIME_DIRECTORY"
    val ENSO_LOG_DIRECTORY     = "ENSO_LOG_DIRECTORY"

    private val XDG_DATA_DIRECTORY   = "XDG_DATA_HOME"
    private val XDG_CONFIG_DIRECTORY = "XDG_CONFIG_HOME"
    private val XDG_BIN_DIRECTORY    = "XDG_BIN_HOME"
    private val XDG_RUN_DIRECTORY    = "XDG_RUNTIME_DIR"
    private val XDG_CACHE_DIRECTORY  = "XDG_CACHE_HOME"

    private val LINUX_ENSO_DIRECTORY   = "enso"
    private val MACOS_ENSO_DIRECTORY   = "org.enso"
    private val WINDOWS_ENSO_DIRECTORY = "enso"

    /**
      * Data directory for an installed distribution.
      */
    def dataDirectory: Path =
      env
        .getEnvPath(ENSO_DATA_DIRECTORY)
        .getOrElse {
          OS.operatingSystem match {
            case OS.Linux =>
              env
                .getEnvPath(XDG_DATA_DIRECTORY)
                .map(_ / LINUX_ENSO_DIRECTORY)
                .getOrElse {
                  env.getHome / ".local" / "share" / LINUX_ENSO_DIRECTORY
                }
            case OS.MacOS =>
              env.getHome / "Library" / "Application Support" / MACOS_ENSO_DIRECTORY
            case OS.Windows =>
              env.getLocalAppData / WINDOWS_ENSO_DIRECTORY
          }
        }
        .toAbsolutePath

    /**
      * Returns names of directories that may be located inside of the data
      * directory.
      */
    def possibleDirectoriesInsideDataDirectory: Seq[String] =
      Seq(
        CONFIG_DIRECTORY,
        TMP_DIRECTORY,
        LOG_DIRECTORY,
        LOCK_DIRECTORY,
        "components-licences"
      )

    /**
      * Config directory for an installed distribution.
      */
    def configDirectory: Path =
      env
        .getEnvPath(ENSO_CONFIG_DIRECTORY)
        .getOrElse {
          OS.operatingSystem match {
            case OS.Linux =>
              env
                .getEnvPath(XDG_CONFIG_DIRECTORY)
                .map(_ / LINUX_ENSO_DIRECTORY)
                .getOrElse {
                  env.getHome / ".config" / LINUX_ENSO_DIRECTORY
                }
            case OS.MacOS =>
              env.getHome / "Library" / "Preferences" / MACOS_ENSO_DIRECTORY
            case OS.Windows =>
              env.getLocalAppData / WINDOWS_ENSO_DIRECTORY / CONFIG_DIRECTORY
          }
        }
        .toAbsolutePath

    /**
      * The directory where the launcher binary will be placed for an installed
      * distribution.
      */
    def binDirectory: Path =
      env
        .getEnvPath(ENSO_BIN_DIRECTORY)
        .getOrElse {
          OS.operatingSystem match {
            case OS.Linux =>
              env
                .getEnvPath(XDG_BIN_DIRECTORY)
                .getOrElse {
                  env.getHome / ".local" / "bin"
                }
            case OS.MacOS =>
              env.getHome / ".local" / "bin"
            case OS.Windows =>
              env.getLocalAppData / WINDOWS_ENSO_DIRECTORY / BIN_DIRECTORY
          }
        }
        .toAbsolutePath

    /**
      * The directory where runtime-synchronization files are stored.
      */
    def runtimeDirectory: Path =
      env
        .getEnvPath(ENSO_RUNTIME_DIRECTORY)
        .getOrElse {
          OS.operatingSystem match {
            case OS.Linux =>
              env
                .getEnvPath(XDG_RUN_DIRECTORY)
                .map(_ / LINUX_ENSO_DIRECTORY)
                .getOrElse(dataDirectory)
            case _ => dataDirectory
          }
        }

    /**
      * The directory for storing logs.
      */
    def logDirectory: Path =
      env
        .getEnvPath(ENSO_LOG_DIRECTORY)
        .getOrElse {
          OS.operatingSystem match {
            case OS.Linux =>
              env
                .getEnvPath(XDG_CACHE_DIRECTORY)
                .map(_ / LINUX_ENSO_DIRECTORY)
                .getOrElse(dataDirectory / LOG_DIRECTORY)
            case OS.MacOS =>
              env.getHome / "Library" / "Logs" / MACOS_ENSO_DIRECTORY
            case OS.Windows =>
              dataDirectory / LOG_DIRECTORY
          }
        }

    private def executableName: String =
      OS.executableName("enso")

    /**
      * The path where the binary executable of the installed distribution
      * should be placed by default.
      */
    def binaryExecutable: Path = {
      binDirectory / executableName
    }

    /**
      * The safe version of [[dataDirectory]] which returns None if the
      * directory cannot be determined.
      *
      * Should be used in places where not being able to determine the data
      * directory is not a fatal error.
      */
    def safeDataDirectory: Option[Path] =
      Try(dataDirectory).toOption

    /**
      * Determines whether a locally installed distribution exists on the
      * system.
      */
    def installedDistributionExists: Boolean =
      safeDataDirectory.exists(Files.isDirectory(_))
  }
}

/**
  * A default DistributionManager using the default environment.
  */
object DistributionManager
    extends DistributionManager(Environment, DefaultResourceManager)
