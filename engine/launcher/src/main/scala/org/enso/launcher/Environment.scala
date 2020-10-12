package org.enso.launcher

import java.io.File
import java.nio.file.Path

import com.typesafe.scalalogging.Logger

import scala.util.Try

/**
  * Gathers some helper methods querying the system environment.
  *
  * The default implementations should be used most of the time, but it is a
  * trait so that the functions can be overridden in tests.
  */
trait Environment {

  /**
    * Returns a list of system-dependent plugin extensions.
    *
    * By default, on Unix plugins should have no extensions. On Windows, `.exe`
    * `.bat` and `.cmd` are supported.
    */
  def getPluginExtensions: Seq[String] =
    if (OS.isWindows)
      Seq(".exe", ".bat", ".cmd")
    else Seq()

  /**
    * Returns a list of directories that can be ignored when traversing the
    * system PATH looking for plugins.
    *
    * These could be system directories that should not contain plguins anyway,
    * but traversing them would greatly slow down plugin discovery.
    */
  def getIgnoredPathDirectories: Seq[Path] =
    if (OS.isWindows) Seq(Path.of("C:\\Windows")) else Seq()

  /**
    * Queries the system environment for the given variable that should
    * represent a valid filesystem path.
    *
    * If it is not defined or is not a valid path, returns None.
    */
  def getEnvPath(key: String): Option[Path] = {
    def parsePathWithWarning(str: String): Option[Path] = {
      val result = safeParsePath(str)
      if (result.isEmpty) {
        Logger[Environment].warn(
          s"System variable `$key` was set (to value `$str`), but it did not " +
          s"represent a valid path, so it has been ignored."
        )
      }

      result
    }

    getEnvVar(key).flatMap(parsePathWithWarning)
  }

  /**
    * Returns the system PATH, if available.
    */
  def getSystemPath: Seq[Path] =
    getEnvVar("PATH")
      .map(_.split(File.pathSeparatorChar).toSeq.flatMap(safeParsePath))
      .getOrElse(Seq())

  /**
    * Returns the location of the HOME directory on Unix systems.
    *
    * Should not be called on Windows, as the concept of HOME should be handled
    * differently there.
    */
  def getHome: Path = {
    if (OS.isWindows)
      throw new IllegalStateException(
        "fatal error: HOME should not be queried on Windows"
      )
    else {
      getEnvVar("HOME").flatMap(safeParsePath) match {
        case Some(path) => path
        case None =>
          throw new RuntimeException(
            "fatal error: HOME environment variable is not defined."
          )
      }
    }
  }

  /**
    * Returns the location of the local application data directory
    * (`%LocalAppData%`) on Windows.
    *
    * Should not be called on platforms other than Windows, as this concept is
    * defined in different ways there.
    */
  def getLocalAppData: Path = {
    if (!OS.isWindows)
      throw new IllegalStateException(
        "fatal error: LocalAppData should be queried only on Windows"
      )
    else {
      getEnvVar("LocalAppData").flatMap(safeParsePath) match {
        case Some(path) => path
        case None =>
          throw new RuntimeException(
            "fatal error: %LocalAppData% environment variable is not defined."
          )
      }
    }
  }

  /**
    * Queries the system environment for the given variable.
    *
    * If it is not defined or empty, returns None.
    */
  def getEnvVar(key: String): Option[String] = {
    val value = System.getenv(key)
    if (value == null || value == "") None
    else Some(value)
  }

  /**
    * Tries to parse a path string and returns Some(path) on success.
    *
    * We prefer silent failures here (returning None and skipping that entry),
    * as we don't want to fail the whole command if the PATH contains some
    * unparseable entries.
    */
  private def safeParsePath(str: String): Option[Path] =
    Try(Path.of(str)).toOption

  /**
    * Returns the path to the running program.
    *
    * It is intended for usage in native binary builds, where it returns the
    * path to the binary executable that is running. When running on the JVM,
    * returns a path to the root of the classpath for the `org.enso.launcher`
    * package or a built JAR.
    */
  def getPathToRunningExecutable: Path
}

/**
  * The default [[Environment]] implementation.
  */
object Environment extends Environment {

  /**
    * @inheritdoc
    */
  override def getPathToRunningExecutable: Path =
    executablePathOverride.getOrElse(executablePath)

  private def executablePath: Path =
    try {
      val codeSource =
        this.getClass.getProtectionDomain.getCodeSource
      Path.of(codeSource.getLocation.toURI).toAbsolutePath
    } catch {
      case e: Exception =>
        throw new IllegalStateException(
          "Cannot locate the path of the launched executable",
          e
        )
    }

  private var executablePathOverride: Option[Path] = None

  /**
    * Overrides the return value of [[getPathToRunningExecutable]] with the
    * provided path.
    *
    * Internal method used for testing. It should be called as early as
    * possible, before [[getPathToRunningExecutable]] is called.
    */
  def internalOverrideExecutableLocation(newLocation: Path): Unit =
    if (buildinfo.Info.isRelease)
      throw new IllegalStateException(
        "Internal testing function internalOverrideExecutableLocation used " +
        "in a release build."
      )
    else {
      Logger("TEST").debug(s"Overriding location to $newLocation.")
      executablePathOverride = Some(newLocation)
    }
}
