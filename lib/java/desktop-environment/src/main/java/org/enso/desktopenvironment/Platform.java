package org.enso.desktopenvironment;

public final class Platform {

  private static final String OS_NAME = "os.name";
  private static final String LINUX = "linux";
  private static final String MAC = "mac";
  private static final String WINDOWS = "windows";

  private static final OS OPERATING_SYSTEM = detectOperatingSystem();

  private Platform() {}

  public enum OS {
    LINUX,
    MACOS,
    WINDOWS
  }

  private static OS detectOperatingSystem() {
    var osName = System.getProperty(OS_NAME);
    var lowerOsName = osName.toLowerCase();

    if (lowerOsName.contains(LINUX)) {
      return OS.LINUX;
    }

    if (lowerOsName.contains(MAC)) {
      return OS.MACOS;
    }

    if (lowerOsName.contains(WINDOWS)) {
      return OS.WINDOWS;
    }

    throw new IllegalStateException("Unknown Operrating System: '" + osName + "'");
  }

  public static OS getOperatingSystem() {
    return OPERATING_SYSTEM;
  }

  public static boolean isLinux() {
    return OPERATING_SYSTEM == OS.LINUX;
  }

  public static boolean isMacOs() {
    return OPERATING_SYSTEM == OS.MACOS;
  }

  public static boolean isWindows() {
    return OPERATING_SYSTEM == OS.WINDOWS;
  }

  public static Directories getDirectories() {
    return DirectoriesFactory.getInstance();
  }

  public static Trash getTrash() {
    return TrashFactory.getInstance();
  }
}
