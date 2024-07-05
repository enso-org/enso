package org.enso.desktopenvironment;

import java.awt.Desktop;

final class TrashFactory {

  private static final class LazyTrash {

    private static Trash Instance = null;

    private LazyTrash() {}

    public static Trash getInstance() {
      if (Instance == null) {
        Instance = switch (Platform.getOperatingSystem()) {
          case Platform.OS.LINUX -> new LinuxTrash();
          case Platform.OS.MACOS, Platform.OS.WINDOWS -> new AwtTrash();
        };
      }
      return Instance;
    }
  }

  private TrashFactory() {}

  /*
  private static Trash initTrash() {
    if (Desktop.isDesktopSupported()) {
      var desktop = Desktop.getDesktop();
      if (desktop.isSupported(Desktop.Action.MOVE_TO_TRASH)) {
        return new AwtTrash(desktop);
      }
    }

    if (Platform.isLinux()) {
      return new LinuxTrash();
    }

    return new UnsupportedTrash();
  }
   */

  public static Trash getInstance() {
    return LazyTrash.getInstance();
  }
}
