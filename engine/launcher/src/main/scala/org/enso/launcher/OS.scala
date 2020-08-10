package org.enso.launcher

sealed trait OS {
  def name: String
}
object OS {
  case object Linux extends OS {
    def name: String = "linux"
  }
  case object MacOS extends OS {
    def name: String = "mac"
  }
  case object Windows extends OS {
    def name: String = "windows"
  }

  /**
    * Checks if the application is being run on Windows.
    */
  def isWindows: Boolean =
    operatingSystem == OS.Windows

  def isUNIX: Boolean =
    operatingSystem == OS.Linux || operatingSystem == OS.MacOS

  /**
    * Returns which [[OS]] this program is running on.
    */
  lazy val operatingSystem: OS = detectOS

  private val ENSO_OPERATING_SYSTEM = "ENSO_OPERATING_SYSTEM"

  private def detectOS: OS = {
    val knownOS       = Seq(Linux, MacOS, Windows)
    val overridenName = Option(System.getenv(ENSO_OPERATING_SYSTEM))
    overridenName match {
      case Some(value) =>
        knownOS.find(_.name == value.toLowerCase) match {
          case Some(overriden) =>
            Logger.debug(
              s"OS overriden by $ENSO_OPERATING_SYSTEM to $overriden."
            )
            return overriden
          case None =>
            Logger.warn(
              s"$ENSO_OPERATING_SYSTEM is set to an unknown value $value, " +
              s"ignoring."
            )
        }
      case None =>
    }

    val name                         = System.getProperty("os.name").toLowerCase
    def nameMatches(os: OS): Boolean = name.contains(os.name)
    val possibleOS                   = knownOS.filter(nameMatches)
    if (possibleOS.length == 1) {
      possibleOS.head
    } else {
      Logger.error(
        s"Could not determine a supported operating system. Please make sure " +
        s"the OS you are running is supported. You can try to manually " +
        s"override the operating system detection by setting an environment " +
        s"variable `$ENSO_OPERATING_SYSTEM` to one of the possible values " +
        s"`linux`, `mac`, `windows` depending on the system that your OS " +
        s"most behaves like."
      )
      throw new IllegalStateException(
        "fatal: Could not detect the operating system."
      )
    }
  }

  /**
    * Name of the architecture that the program is running on.
    *
    * Currently the Launcher Native Image builds only support amd64
    * architecture, so it is hardcoded here. In the future, more architectures
    * may be supported. In that case, this will need to be updated to get the
    * target architecture from the build settings.
    *
    * This property should not use `System.getProperty("os.arch")` directly
    * because it can return different values for the same architecture (for
    * example on some systems `amd64` is called `x86_64`).
    */
  val architecture: String = "amd64"

  /**
    * Wraps the base executable name with an optional platform-dependent
    * extension.
    */
  def executableName(baseName: String): String =
    if (isWindows) baseName + ".exe" else baseName
}
