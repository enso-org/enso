package org.enso.desktopenvironment;

import org.enso.common.Platform;

final class DesktopEnvironment {
  private static final Directories DIRECTORIES;
  private static final TrashBin TRASH_BIN;

  static {
    switch (Platform.getOperatingSystem()) {
      case LINUX -> {
        DIRECTORIES = LinuxDirectories.getInstance();
        TRASH_BIN = LinuxTrashBin.getInstance();
      }
      case MACOS -> {
        DIRECTORIES = MacOsDirectories.getInstance();
        TRASH_BIN = MacTrashBin.getInstance();
      }
      case WINDOWS -> {
        DIRECTORIES = WindowsDirectories.getInstance();
        TRASH_BIN = WindowsTrashBin.getInstance();
      }
      default -> {
        DIRECTORIES = null;
        TRASH_BIN = null;
      }
    }
  }

  private DesktopEnvironment() {}

  public static Directories getDirectories() {
    return DIRECTORIES;
  }

  public static TrashBin getTrashBin() {
    return TRASH_BIN;
  }
}
