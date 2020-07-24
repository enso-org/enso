package org.enso.launcher.internal

sealed trait OS
object OS {
  case object Linux   extends OS
  case object MacOS   extends OS
  case object Windows extends OS

  /**
    * Checks if the application is being run on Windows.
    */
  def isWindows: Boolean =
    operatingSystem == OS.Windows

  /**
    * Returns which [[OS]] this program is running on.
    */
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
    * Wraps the base executable name with an optional platform-dependent
    * extension.
    */
  def executableName(baseName: String): String =
    if (isWindows) baseName + ".exe" else baseName
}
