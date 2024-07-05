package org.enso.desktopenvironment;

import java.awt.Desktop;

final class TrashFactory {

  private static Trash Instance = null;

  private TrashFactory() {}

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

  public static synchronized Trash getInstance() {
    if (Instance == null) {
      Instance = initTrash();
    }
    return Instance;
  }
}
