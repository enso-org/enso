import sbt.singleFileFinder

import java.io.File

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

  def isAmd64: Boolean = {
    val arch = sys.props("os.arch").toLowerCase()
    arch.contains("amd64") || arch.contains("x86_64")
  }

  def isArm64: Boolean =
    sys.props("os.arch").toLowerCase().contains("aarch64")

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

  /** Returns the executable file on the current platform.
    *
    * @param file the generic executable path
    * @return the file corresponding to the provided executable on the current platform
    */
  def executableFile(file: File): String =
    if (isWindows) {
      val parent = file.getParentFile
      if (parent == null) s".\\${file.getName}.bat"
      else if (parent.isAbsolute)
        new File(parent, s"${file.getName}.bat").getPath
      else s".\\${parent.getPath}${file.getPath}.bat"
    } else file.getPath
}
