package org.enso.desktopenvironment;

import java.awt.Desktop;

final class TrashFactory {

  private static final Trash Instance = initTrash();

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

  public static Trash getInstance() {
    return Instance;
  }
}
