package org.enso.desktopenvironment;

public final class Platform {

  private static final String OS_NAME = "os.name";
  private static final String LINUX = "linux";
  private static final String MAC = "mac";
  private static final String WINDOWS = "windows";

  private static final Directories DIRECTORIES = initDirectories();

  private Platform() {}

  public static Directories getDirectories() {
    return DIRECTORIES;
  }

  private static Directories initDirectories() {
    if (isLinux()) {
      return new LinuxDirectories();
    }

    if (isMacOs()) {
      return new MacOsDirectories();
    }

    if (isWindows()) {
      return new WindowsDirectories();
    }

    throw new UnsupportedOperationException("Unsupported OS '" + getOsName() + "'");
  }

  private static String getOsName() {
    return System.getProperty(OS_NAME);
  }

  public static boolean isLinux() {
    return System.getProperty(OS_NAME).toLowerCase().contains(LINUX);
  }

  public static boolean isMacOs() {
    return System.getProperty(OS_NAME).toLowerCase().contains(MAC);
  }

  public static boolean isWindows() {
    return System.getProperty(OS_NAME).toLowerCase().contains(WINDOWS);
  }
}
