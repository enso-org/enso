package org.enso.launcher.internal

import org.enso.launcher.Logger

sealed trait OS
object OS {
  case object Linux   extends OS
  case object MacOS   extends OS
  case object Windows extends OS

  /**
    * The special case for when the OS is not detected.
    *
    * It is treated as a UNIX system.
    */
  case object Unknown extends OS

  /**
    * Checks if the application is being run on Windows.
    */
  def isWindows: Boolean =
    operatingSystem == OS.Windows

  /**
    * Returns which [[OS]] this program is running on.
    */
  lazy val operatingSystem: OS = detectOS

  private def detectOS: OS = {
    val name = System.getProperty("os.name").toLowerCase
    def nameMatches(os: OS): Boolean =
      os match {
        case Linux   => name.contains("linux")
        case MacOS   => name.contains("mac")
        case Windows => name.contains("windows")
        case Unknown => false
      }

    val knownOS    = Seq(Linux, MacOS, Windows)
    val possibleOS = knownOS.filter(nameMatches)
    if (possibleOS.length == 1) {
      possibleOS.head
    } else {
      Logger.warn(
        s"Could not determine a supported operating system. Assuming a UNIX " +
        s"system. Things may not work correctly."
      )
      Unknown
    }
  }

  /**
    * Wraps the base executable name with an optional platform-dependent
    * extension.
    */
  def executableName(baseName: String): String =
    if (isWindows) baseName + ".exe" else baseName
}
