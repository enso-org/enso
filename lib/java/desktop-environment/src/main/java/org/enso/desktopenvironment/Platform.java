package org.enso.desktopenvironment;

import org.enso.desktopenvironment.directories.Directories;
import org.enso.desktopenvironment.directories.DirectoriesFactory;

public final class Platform {

  private static final String OS_NAME = "os.name";
  private static final String LINUX = "linux";
  private static final String MAC = "mac";
  private static final String WINDOWS = "windows";

  private Platform() {}

  public static Directories getDirectories() {
    return DirectoriesFactory.getInstance();
  }

  public static String getOsName() {
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
