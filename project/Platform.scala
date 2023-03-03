object Platform {

  /** Returns true if the build system is running on Windows.
    */
  def isWindows: Boolean =
    sys.props("os.name").toLowerCase().contains("windows")

  /** Returns true if the build system is running on Linux.
    */
  def isLinux: Boolean =
    sys.props("os.name").toLowerCase().contains("linux")

  /** Returns true if the build system is running on macOS.
    */
  def isMacOS: Boolean =
    sys.props("os.name").toLowerCase().contains("mac")

  /** Returns the dynamic library file name on the current platform.
    *
    * @param libraryName the library name
    * @return the file name of provided library on the current platform
    */
  def dynamicLibraryFileName(libraryName: String): String = {
    if (isMacOS) s"lib$libraryName.dylib"
    else if (isWindows) s"$libraryName.dll"
    else if (isLinux) s"lib$libraryName.so"
    else {
      throw new RuntimeException(s"Unknown platform [${sys.props("os.name")}].")
    }
  }

  /** Returns the executable file name on the current platform.
    *
    * @param name the executable name
    * @return the file name of provided executable on the current platform
    */
  def executableFileName(name: String): String = {
    if (isWindows) s".\\$name.bat" else name
  }

}
