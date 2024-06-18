package org.enso.desktopenvironment.directories;

import org.enso.desktopenvironment.Platform;

public final class DirectoriesFactory {

  private static final Directories INSTANCE = initDirectories();

  private static Directories initDirectories() {
    if (Platform.isLinux()) {
      return new LinuxDirectories();
    }

    if (Platform.isMacOs()) {
      return new MacOsDirectories();
    }

    if (Platform.isWindows()) {
      return new WindowsDirectories();
    }

    throw new UnsupportedOperationException("Unsupported OS '" + Platform.getOsName() + "'");
  }

  private DirectoriesFactory() {}

  public static Directories getInstance() {
    return INSTANCE;
  }
}
