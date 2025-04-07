package org.enso.common;

public enum Platform {
  LINUX,
  MACOS,
  WINDOWS;

  private static final String OS_NAME = "os.name";
  private static final String OS_LINUX = "linux";
  private static final String OS_MAC = "mac";
  private static final String OS_WINDOWS = "windows";

  private static final Platform OPERATING_SYSTEM = detectOperatingSystem();

  private static Platform detectOperatingSystem() {
    var osName = System.getProperty(OS_NAME);
    var lowerOsName = osName.toLowerCase();

    if (lowerOsName.contains(OS_LINUX)) {
      return LINUX;
    }

    if (lowerOsName.contains(OS_MAC)) {
      return MACOS;
    }

    if (lowerOsName.contains(OS_WINDOWS)) {
      return WINDOWS;
    }

    throw new IllegalStateException("Unknown Operrating System: '" + osName + "'");
  }

  public static Platform getOperatingSystem() {
    return OPERATING_SYSTEM;
  }

  /**
   * Has the same behavior as {@link java.io.File#separatorChar}. Use this method if you want to
   * avoid initializing {@link java.io.File} class.
   *
   * @return the file separator character for the current operating system.
   */
  public static char separatorChar() {
    return switch (OPERATING_SYSTEM) {
      case LINUX, MACOS -> '/';
      case WINDOWS -> '\\';
    };
  }

  public boolean isLinux() {
    return this == LINUX;
  }

  public boolean isMacOs() {
    return this == MACOS;
  }

  public boolean isWindows() {
    return this == WINDOWS;
  }
}
