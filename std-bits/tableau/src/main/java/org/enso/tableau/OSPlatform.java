package org.enso.tableau;

public enum OSPlatform {
  WINDOWS,
  MAC_ARM64,
  MAX_X64,
  LINUX,
  OTHER;

  /** Returns the current platform. */
  public static final OSPlatform CurrentPlatform = getPlatform();

  private static OSPlatform getPlatform() {
    var osName = System.getProperty("os.name").toUpperCase();
    if (osName.contains("WIN")) {
      return OSPlatform.WINDOWS;
    } else if (osName.contains("MAC")) {
      var osArch = System.getProperty("os.arch").toUpperCase();
      if (osArch.contains("ARM64") || osArch.contains("AARCH64")) {
        return OSPlatform.MAC_ARM64;
      } else {
        return OSPlatform.MAX_X64;
      }
    } else if (osName.contains("LINUX")) {
      return OSPlatform.LINUX;
    } else {
      return OSPlatform.OTHER;
    }
  }
}
