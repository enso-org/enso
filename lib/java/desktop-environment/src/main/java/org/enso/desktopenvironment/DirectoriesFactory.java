package org.enso.desktopenvironment;

final class DirectoriesFactory {

  private static final Directories INSTANCE = initDirectories();

  private static Directories initDirectories() {
    return switch (Platform.getOperatingSystem()) {
      case LINUX -> new LinuxDirectories();
      case MACOS -> new MacOsDirectories();
      case WINDOWS -> new WindowsDirectories();
    };
  }

  private DirectoriesFactory() {}

  public static Directories getInstance() {
    return INSTANCE;
  }
}
