package org.enso.launcher

import java.io.File
import java.nio.file.Path

import scala.util.Try

sealed trait OS
object OS {
  case object Linux   extends OS
  case object MacOS   extends OS
  case object Windows extends OS
}

object Environment {

  /**
    * Returns the system PATH, if available.
    */
  def getSystemPath: Seq[Path] =
    Option(System.getenv("PATH"))
      .map(_.split(File.pathSeparatorChar).toSeq.flatMap(safeParsePath))
      .getOrElse(Seq())

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
    * Checks if the application is being run on Windows.
    */
  def isWindows: Boolean =
    operatingSystem == OS.Windows

  def operatingSystem: OS = {
    val name = System.getProperty("os.name").toLowerCase
    if (name.contains("linux")) OS.Linux
    else if (name.contains("mac")) OS.MacOS
    else if (name.contains("windows")) OS.Windows
    else
      throw new RuntimeException(
        s"fatal error: os.name `$name` is not recognized."
      )
  }

  /**
    * Returns a list of system-dependent plugin extensions.
    *
    * By default, on Unix plugins should have no extensions. On Windows, `.exe`
    * `.bat` and `.cmd` are supported.
    */
  def getPluginExtensions: Seq[String] =
    if (isWindows)
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
    if (isWindows) Seq(Path.of("C:\\Windows")) else Seq()
}
