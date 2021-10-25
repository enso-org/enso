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
}
