package org.enso.desktopenvironment;

public final class Platform {

  private static final String OS_NAME = "os.name";
  private static final String LINUX = "linux";
  private static final String MAC = "mac";
  private static final String WINDOWS = "windows";

  private Platform() {}

  public static String getOsName() {
    return System.getProperty(OS_NAME);
  }

  public static boolean isLinux() {
    return getOsName().toLowerCase().contains(LINUX);
  }

  public static boolean isMacOs() {
    return getOsName().toLowerCase().contains(MAC);
  }

  public static boolean isWindows() {
    return getOsName().toLowerCase().contains(WINDOWS);
  }

  public static Directories getDirectories() {
    return DirectoriesFactory.getInstance();
  }
}
