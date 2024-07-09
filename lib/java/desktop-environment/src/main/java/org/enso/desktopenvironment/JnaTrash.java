package org.enso.desktopenvironment;

import com.sun.jna.platform.FileUtils;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * JNA trash implementation calling native platform-dependent APIs.
 *
 * <p>Native {@link FileUtils} are implemented for Windows and macOS platforms.
 */
final class JnaTrash implements Trash {

  @Override
  public boolean isSupported() {
    return FileUtils.getInstance().hasTrash();
  }

  @Override
  public boolean moveToTrash(Path path) {
    if (Files.exists(path) && isSupported()) {
      try {
        FileUtils.getInstance().moveToTrash(path.toFile());

        return true;
      } catch (IOException ignored) {
        return false;
      }
    }

    return false;
  }
}
