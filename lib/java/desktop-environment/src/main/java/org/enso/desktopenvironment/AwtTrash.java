package org.enso.desktopenvironment;

import java.awt.Desktop;
import java.nio.file.Files;
import java.nio.file.Path;

/** The {@link Trash} implementation provided by Java Abstract Window Toolkit. */
final class AwtTrash implements Trash {

  private final Desktop desktop;

  AwtTrash(Desktop desktop) {
    this.desktop = desktop;
  }

  @Override
  public boolean isSupported() {
    return desktop.isSupported(Desktop.Action.MOVE_TO_TRASH);
  }

  @Override
  public boolean moveToTrash(Path path) {
    if (Files.exists(path) && isSupported()) {
      return desktop.moveToTrash(path.toFile());
    } else {
      return false;
    }
  }
}
