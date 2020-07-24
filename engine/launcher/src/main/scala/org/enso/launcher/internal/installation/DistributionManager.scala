package org.enso.launcher.internal.installation

import java.nio.file.{Files, Path}

import org.enso.launcher.Logger
import org.enso.launcher.FileSystem.PathSyntax
import org.enso.launcher.internal.{Environment, OS}

/**
  * @param runtimes location of runtimes, corresponding to `runtime` directory
  * @param engines location of engine versions, corresponding to `dist`
  *                directory
  * @param config location of configuration
  * @param dataRoot the root of the data directory; for a portable distribution
  *                 this is the root of the distribution, for a locally
  *                 installed distribution, it corresponds to `ENSO_DATA_DIR`
  */
case class DistributionPaths(
  dataRoot: Path,
  runtimes: Path,
  engines: Path,
  config: Path
) {
  override def toString: String =
    s"""DistributionPaths(
       |  dataRoot = $dataRoot,
       |  runtimes = $runtimes,
       |  engines  = $engines,
       |  config   = $config
       |)""".stripMargin
}

class DistributionManager(val env: Environment) {

  /**
    * Specifies whether the launcher has been run as a portable distribution or
    * it is a locally installed distribution.
    */
  lazy val isRunningPortable: Boolean = {
    val portable = detectPortable()
    Logger.debug(s"Launcher portable mode = $portable")
    if (portable && LocallyInstalledDirectories.installedDistributionExists) {
      val installedRoot   = LocallyInstalledDirectories.dataDirectory
      val installedBinary = LocallyInstalledDirectories.binaryExecutable

      Logger.debug(
        s"The launcher is run in portable mode, but an installed distribution" +
        s" is available at $installedRoot."
      )

      if (Files.exists(installedBinary)) {
        if (installedBinary == getPathToRunningBinaryExecutable) {
          Logger.debug(
            "That distribution seems to be corresponding to this launcher " +
            "executable, that is running in portable mode."
          )
        } else {
          Logger.debug(
            s"However, that installed distribution most likely uses another " +
            s"launcher executable, located at $installedBinary."
          )
        }
      }
    }
    portable
  }

  lazy val paths: DistributionPaths = {
    val paths = detectPaths()
    Logger.debug(s"Detected paths are: $paths")
    paths
  }

  private val PORTABLE_MARK_FILENAME = ".enso.portable"
  val ENGINES_DIRECTORY              = "dist"
  val RUNTIMES_DIRECTORY             = "runtime"
  val CONFIG_DIRECTORY               = "config"
  val BIN_DIRECTORY                  = "bin"

  private def detectPortable(): Boolean = Files.exists(portableMarkFilePath)
  private def possiblePortableRoot: Path =
    getPathToRunningBinaryExecutable.getParent.getParent

  def portableMarkFilePath: Path =
    possiblePortableRoot / PORTABLE_MARK_FILENAME

  private def detectPaths(): DistributionPaths =
    if (isRunningPortable) {
      val root = getPathToRunningBinaryExecutable.getParent.getParent
      DistributionPaths(
        dataRoot = root,
        runtimes = root / RUNTIMES_DIRECTORY,
        engines  = root / ENGINES_DIRECTORY,
        config   = root / CONFIG_DIRECTORY
      )
    } else {
      val dataRoot   = LocallyInstalledDirectories.dataDirectory
      val configRoot = LocallyInstalledDirectories.configDirectory
      DistributionPaths(
        dataRoot = dataRoot,
        runtimes = dataRoot / RUNTIMES_DIRECTORY,
        engines  = dataRoot / ENGINES_DIRECTORY,
        config   = configRoot
      )
    }

  def getPathToRunningBinaryExecutable: Path = {
    val codeSource =
      this.getClass.getProtectionDomain.getCodeSource
    Path.of(codeSource.getLocation.getPath).toAbsolutePath
  }

  object LocallyInstalledDirectories {
    val ENSO_DATA_DIRECTORY   = "ENSO_DATA_DIRECTORY"
    val ENSO_CONFIG_DIRECTORY = "ENSO_CONFIG_DIRECTORY"
    val ENSO_BIN_DIRECTORY    = "ENSO_BIN_DIRECTORY"

    private val XDG_DATA_DIRECTORY   = "XDG_DATA_DIRECTORY"
    private val XDG_CONFIG_DIRECTORY = "XDG_CONFIG_DIRECTORY"
    private val XDG_BIN_DIRECTORY    = "XDG_BIN_DIRECTORY"

    private val LINUX_ENSO_DIRECTORY   = "enso"
    private val MACOS_ENSO_DIRECTORY   = "org.enso"
    private val WINDOWS_ENSO_DIRECTORY = "enso"

    private val UNIX_EXECUTABLE_NAME    = "enso"
    private val WINDOWS_EXECUTABLE_NAME = "enso.exe"

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

    private def executableName: String =
      if (OS.operatingSystem == OS.Windows) WINDOWS_EXECUTABLE_NAME
      else UNIX_EXECUTABLE_NAME

    def binaryExecutable: Path = {
      binDirectory / executableName
    }

    def installedDistributionExists: Boolean = {
      val exists = Files.isDirectory(dataDirectory.toAbsolutePath)
      Logger.debug(s"installed root = $dataDirectory, exists? $exists")

      exists
    }
  }
}
