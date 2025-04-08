package org.enso.os.environment.trash;

import java.nio.file.Path;
import org.enso.common.Platform;

/** Operations with system trash */
public sealed interface TrashBin permits LinuxTrashBin, WindowsTrashBin, MacTrashBin {

  static TrashBin getCurrent() {
    return switch (Platform.getOperatingSystem()) {
      case LINUX -> LinuxTrashBin.getInstance();
      case WINDOWS -> WindowsTrashBin.getInstance();
      case MACOS -> MacTrashBin.getInstance();
    };
  }

  /**
   * @return {@code true} if the trash functionality is supported on this platform.
   */
  boolean isSupported();

  /**
   * Move the specified path to the trash bin.
   *
   * @param path the file path.
   * @return {@code true} if the operation was successful.
   */
  boolean moveToTrash(Path path);
}
