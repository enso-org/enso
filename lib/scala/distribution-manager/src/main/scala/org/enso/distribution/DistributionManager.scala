package org.enso.distribution

import com.typesafe.scalalogging.Logger
import org.enso.distribution.FileSystem.PathSyntax

import java.nio.file.{Files, Path}
import scala.util.Try
import scala.util.control.NonFatal

/** Gathers filesystem paths used by the launcher.
  *
  * @param dataRoot the root of the data directory; for a portable distribution
  *                 this is the root of the distribution, for a locally
  *                 installed distribution, it corresponds to `ENSO_DATA_DIR`
  * @param runtimes primary location of runtimes, corresponding to `runtime`
  *                 directory
  * @param engines primary location of engine versions, corresponding to `dist`
  *                directory
  * @param bundle optional bundle description, containing secondary engine and
  *               runtime directories
  * @param config location of configuration
  * @param locks a directory for storing lockfiles that are used to synchronize
  *              access to the various components
  * @param logs a directory for storing logs
  * @param unsafeTemporaryDirectory path to the temporary directory, should not
  *                                 be used directly, see
  *                                 [[TemporaryDirectoryManager]]
  * @param customEditions the search paths for editions
  * @param localLibrariesSearchPaths a sequence of paths to search for local
  *                                  libraries, in order of precedence
  * @param ensoHome the home directory for user's projects etc.
  */
case class DistributionPaths(
  dataRoot: Path,
  runtimes: Path,
  engines: Path,
  bundle: Option[Bundle],
  config: Path,
  locks: Path,
  logs: Path,
  unsafeTemporaryDirectory: Path,
  customEditions: Seq[Path],
  localLibrariesSearchPaths: Seq[Path],
  ensoHome: Path
) {

  /** @inheritdoc */
  override def toString: String =
    s"""DistributionPaths(
       |  dataRoot = $dataRoot,
       |  runtimes = $runtimes,
       |  engines  = $engines,
       |  bundle   = $bundle,
       |  config   = $config,
       |  locks    = $locks,
       |  tmp      = $unsafeTemporaryDirectory,
       |  ensoHome = $ensoHome
       |)""".stripMargin

  /** Sequence of paths to search for engine installations, in order of
    * precedence.
    */
  def engineSearchPaths: Seq[Path] = Seq(engines) ++ bundle.map(_.engines).toSeq

  /** Sequence of paths to search for runtime installations, in order of
    * precedence.
    */
  def runtimeSearchPaths: Seq[Path] =
    Seq(runtimes) ++ bundle.map(_.runtimes).toSeq

  /** The directory for cached editions managed by us. */
  def cachedEditions: Path = dataRoot / DistributionManager.EDITIONS_DIRECTORY

  /** The directory for cached libraries managed by us. */
  def cachedLibraries: Path = dataRoot / DistributionManager.LIBRARIES_DIRECTORY

  /** Sequence of paths to search for edition configurations, in order of
    * precedence.
    */
  def editionSearchPaths: Seq[Path] =
    customEditions ++ Seq(cachedEditions)
}

/** Paths to secondary directories for additionally bundled engine
  * distributions.
  *
  * These paths are only relevant for an installed distribution which may also
  * use some locally bundled distributions (possibly located on a read-only
  * filesystem).
  *
  * For portable distributions, bundled packages are already included in the
  * primary directory.
  */
case class Bundle(engines: Path, runtimes: Path)

/** A helper class that encapsulates management of paths to components of the
  * distribution.
  *
  * It defaults to using the locally installed mode. If portable distributions
  * should be supported, the specialized [[PortableDistributionManager]] should
  * be used.
  */
class DistributionManager(val env: Environment) {
  private val logger = Logger[DistributionManager]
  import DistributionManager._

  /** Determines paths that should be used by the launcher.
    */
  lazy val paths: DistributionPaths = {
    val paths = detectPaths()
    logger.debug("Detected paths: {}", paths)
    paths
  }

  protected def detectPaths(): DistributionPaths = {
    val dataRoot   = LocallyInstalledDirectories.dataDirectory
    val configRoot = LocallyInstalledDirectories.configDirectory
    val runRoot    = LocallyInstalledDirectories.runtimeDirectory
    val home       = detectEnsoHome()
    DistributionPaths(
      dataRoot                  = dataRoot,
      runtimes                  = dataRoot / RUNTIMES_DIRECTORY,
      engines                   = dataRoot / ENGINES_DIRECTORY,
      bundle                    = detectBundle(),
      config                    = configRoot,
      locks                     = runRoot / LOCK_DIRECTORY,
      logs                      = LocallyInstalledDirectories.logDirectory,
      unsafeTemporaryDirectory  = dataRoot / TMP_DIRECTORY,
      customEditions            = detectCustomEditionPaths(home),
      localLibrariesSearchPaths = detectLocalLibraryPaths(home),
      ensoHome                  = home
    )
  }

  private val ENSO_HOME         = "ENSO_HOME"
  private val ENSO_EDITION_PATH = "ENSO_EDITION_PATH"
  private val ENSO_LIBRARY_PATH = "ENSO_LIBRARY_PATH"

  /** Finds the path to the ENSO_HOME directory that is used for keeping user's
    * projects, libraries and other custom artifacts.
    */
  protected def detectEnsoHome(): Path =
    env.getEnvPath(ENSO_HOME).getOrElse(env.getUserProfile / "enso")

  /** Finds the paths to look for custom editions, which may be overridden by
    * setting the ENSO_EDITION_PATH environment variable.
    */
  protected def detectCustomEditionPaths(ensoHome: Path): Seq[Path] =
    env
      .getEnvPaths(ENSO_EDITION_PATH)
      .getOrElse {
        Seq(ensoHome / DistributionManager.Home.EDITIONS_DIRECTORY)
      }

  /** Finds the paths to look for local libraries, which may be overridden by
    * setting the ENSO_LIBRARY_PATH environment variable.
    */
  protected def detectLocalLibraryPaths(ensoHome: Path): Seq[Path] =
    env
      .getEnvPaths(ENSO_LIBRARY_PATH)
      .getOrElse {
        Seq(ensoHome / DistributionManager.Home.LIBRARIES_DIRECTORY)
      }

  /** Name of the file that should be placed in the distribution root to mark it
    * as running in portable mode.
    */
  private val BUNDLE_MARK_FILENAME = ".enso.bundle"

  /** Root directory of a bundle.
    *
    * If the bundle is present, it will be located next to the `bin/` directory
    * that contains the executable that we are currently running.
    */
  private def possibleBundleRoot =
    env.getPathToRunningExecutable.getParent.getParent

  /** Checks if [[possibleBundleRoot]] contains the bundle mark file and returns
    * directories for the bundle if it was found.
    */
  private def detectBundle(): Option[Bundle] =
    if (Files.exists(possibleBundleRoot / BUNDLE_MARK_FILENAME))
      Some(
        Bundle(
          engines  = possibleBundleRoot / ENGINES_DIRECTORY,
          runtimes = possibleBundleRoot / RUNTIMES_DIRECTORY
        )
      )
    else None

  /** Removes unused lockfiles. */
  def tryCleaningUnusedLockfiles(): Unit = {
    val lockfiles = FileSystem.listDirectory(paths.locks)
    for (lockfile <- lockfiles) {
      try {
        Files.delete(lockfile)
        logger.debug("Removed unused lockfile [{}].", lockfile.getFileName)
      } catch {
        case NonFatal(_) =>
      }
    }
  }

  /** A helper for managing directories of the non-portable installation.
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

    /** Data directory for an installed distribution.
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

    /** Returns names of directories that may be located inside of the data
      * directory.
      */
    def possibleDirectoriesInsideDataDirectory: Seq[String] =
      Seq(
        CONFIG_DIRECTORY,
        TMP_DIRECTORY,
        LOG_DIRECTORY,
        LOCK_DIRECTORY,
        "THIRD-PARTY"
      ) ++ FileSystem.ignoredFileNames

    /** Config directory for an installed distribution. */
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

    /** The directory where the launcher binary will be placed for an installed
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

    /** The directory where runtime-synchronization files are stored. */
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

    /** The directory for storing logs. */
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

    /** The path where the binary executable of the installed distribution
      * should be placed by default.
      */
    def binaryExecutable: Path = {
      binDirectory / executableName
    }

    /** The safe version of [[dataDirectory]] which returns None if the
      * directory cannot be determined.
      *
      * Should be used in places where not being able to determine the data
      * directory is not a fatal error.
      */
    def safeDataDirectory: Option[Path] =
      Try(dataDirectory).toOption

    /** Determines whether a locally installed distribution exists on the
      * system.
      */
    def installedDistributionExists: Boolean =
      safeDataDirectory.exists(Files.isDirectory(_))
  }
}

/** A helper object that contains constants defining names of various
  * directories used by Enso components.
  */
object DistributionManager {
  val ENGINES_DIRECTORY   = "dist"
  val RUNTIMES_DIRECTORY  = "runtime"
  val CONFIG_DIRECTORY    = "config"
  val BIN_DIRECTORY       = "bin"
  val LOCK_DIRECTORY      = "lock"
  val LOG_DIRECTORY       = "log"
  val TMP_DIRECTORY       = "tmp"
  val EDITIONS_DIRECTORY  = "editions"
  val LIBRARIES_DIRECTORY = "lib"

  /** Defines paths inside of the ENSO_HOME directory. */
  object Home {
    val EDITIONS_DIRECTORY  = "editions"
    val LIBRARIES_DIRECTORY = "libraries"
  }
}
