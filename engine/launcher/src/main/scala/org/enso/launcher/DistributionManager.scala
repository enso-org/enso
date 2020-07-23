package org.enso.launcher

import java.nio.file.{Files, InvalidPathException, Path}

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

object DistributionManager {

  /**
    * Specifies whether the launcher has been run as a portable distribution or
    * it is a locally installed distribution.
    */
  lazy val isRunningPortable: Boolean = {
    val portable = detectPortable()
    Logger.debug(s"Launcher portable mode = $portable")
    if (portable && LocallyInstalledDirectories.installedDistributionExists) {
      val installedRoot = LocallyInstalledDirectories.dataDirectory
      // TODO [RW] should this be a warn or debug level?
      // It is reasonable for people to use both distributions at the same time,
      // so a warning may be annoying.
      Logger.warn(
        s"The launcher is run in portable mode, but an installed distribution" +
        s" is available at $installedRoot."
      )
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

  def portableMarkFilePath: Path = {
    val possibleRoot = getPathToRunningBinaryExecutable.getParent.getParent
    possibleRoot / PORTABLE_MARK_FILENAME
  }

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
      DistributionManager.getClass.getProtectionDomain.getCodeSource
    Path.of(codeSource.getLocation.getPath)
  }

  object LocallyInstalledDirectories {
    private val ENSO_DATA_DIRECTORY   = "ENSO_DATA_DIRECTORY"
    private val ENSO_CONFIG_DIRECTORY = "ENSO_CONFIG_DIRECTORY"
    private val ENSO_BIN_DIRECTORY    = "ENSO_BIN_DIRECTORY"

    private val XDG_DATA_DIRECTORY   = "XDG_DATA_DIRECTORY"
    private val XDG_CONFIG_DIRECTORY = "XDG_CONFIG_DIRECTORY"
    private val XDG_BIN_DIRECTORY    = "XDG_BIN_DIRECTORY"

    private val LINUX_ENSO_DIRECTORY   = "enso"
    private val MACOS_ENSO_DIRECTORY   = "org.enso"
    private val WINDOWS_ENSO_DIRECTORY = "enso"

    private val UNIX_EXECUTABLE_NAME    = "enso"
    private val WINDOWS_EXECUTABLE_NAME = "enso.exe"

    def dataDirectory: Path = {
      getEnvPath(ENSO_DATA_DIRECTORY).getOrElse {
        Environment.operatingSystem match {
          case OS.Linux =>
            getEnvPath(XDG_DATA_DIRECTORY)
              .map(_ / LINUX_ENSO_DIRECTORY)
              .getOrElse {
                getHome / ".local" / "share" / LINUX_ENSO_DIRECTORY
              }
          case OS.MacOS =>
            getHome / "Library" / "Application Support" / MACOS_ENSO_DIRECTORY
          case OS.Windows =>
            getLocalAppData / WINDOWS_ENSO_DIRECTORY
        }
      }
    }

    def configDirectory: Path = {
      getEnvPath(ENSO_CONFIG_DIRECTORY).getOrElse {
        Environment.operatingSystem match {
          case OS.Linux =>
            getEnvPath(XDG_CONFIG_DIRECTORY)
              .map(_ / LINUX_ENSO_DIRECTORY)
              .getOrElse {
                getHome / ".config" / LINUX_ENSO_DIRECTORY
              }
          case OS.MacOS =>
            getHome / "Library" / "Preferences" / MACOS_ENSO_DIRECTORY
          case OS.Windows =>
            getLocalAppData / WINDOWS_ENSO_DIRECTORY / CONFIG_DIRECTORY
        }
      }
    }

    def binDirectory: Path = {
      getEnvPath(ENSO_BIN_DIRECTORY).getOrElse {
        Environment.operatingSystem match {
          case OS.Linux =>
            getEnvPath(XDG_BIN_DIRECTORY)
              .getOrElse {
                getHome / ".local" / "bin"
              }
          case OS.MacOS =>
            getHome / ".local" / "bin"
          case OS.Windows =>
            getLocalAppData / WINDOWS_ENSO_DIRECTORY / BIN_DIRECTORY
        }
      }
    }

    private def executableName: String =
      if (Environment.operatingSystem == OS.Windows) WINDOWS_EXECUTABLE_NAME
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

  private def getEnvVar(key: String): Option[String] = {
    val value = System.getenv(key)
    if (value == null || value == "") None
    else Some(value)
  }

  private def parsePath(string: String): Option[Path] = {
    try {
      Some(Path.of(string))
    } catch {
      case _: InvalidPathException => None
    }
  }

  private def getEnvPath(key: String): Option[Path] = {
    val result = getEnvVar(key).flatMap(parsePath)
    if (result.isEmpty) {
      Logger.warn(
        s"System variable `$key` was set, but it did not represent a valid " +
        s"path, so it has been ignored."
      )
    }
    result
  }

  private def getHome: Path = {
    if (Environment.operatingSystem == OS.Windows)
      throw new IllegalStateException(
        "fatal error: HOME should not be queried on Windows"
      )
    else {
      getEnvVar("HOME").flatMap(parsePath) match {
        case Some(path) => path
        case None =>
          throw new RuntimeException(
            "fatal error: HOME environment variable is not defined."
          )
      }
    }
  }

  private def getLocalAppData: Path = {
    if (Environment.operatingSystem != OS.Windows)
      throw new IllegalStateException(
        "fatal error: LocalAppData should be queried only on Windows"
      )
    else {
      getEnvVar("LocalAppData").flatMap(parsePath) match {
        case Some(path) => path
        case None =>
          throw new RuntimeException(
            "fatal error: %LocalAppData% environment variable is not defined."
          )
      }
    }
  }

  implicit class PathSyntax(val path: Path) extends AnyVal {
    def /(other: String): Path = path.resolve(other)
  }
}
