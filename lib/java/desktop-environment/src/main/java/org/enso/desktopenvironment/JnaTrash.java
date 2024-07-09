package org.enso.desktopenvironment;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import com.sun.jna.platform.FileUtils;

final class JnaTrash implements Trash {

  private static final FileUtils fileUtils = FileUtils.getInstance();

  @Override
  public boolean isSupported() {
    return fileUtils.hasTrash();
  }

  @Override
  public boolean moveToTrash(Path path) {
    if (Files.exists(path) && isSupported()) {
      try {
        fileUtils.moveToTrash(path.toFile());

        return true;
      } catch (IOException ignored) {
        return false;
      }
    }

    return false;
  }
}
