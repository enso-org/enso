package org.enso.desktopenvironment;

import java.nio.file.Path;

final class UnsupportedTrash implements Trash {

  @Override
  public boolean isSupported() {
    return false;
  }

  @Override
  public boolean moveToTrash(Path path) {
    return false;
  }
}
