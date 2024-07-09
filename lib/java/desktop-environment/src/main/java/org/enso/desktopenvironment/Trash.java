package org.enso.desktopenvironment;

import java.nio.file.Path;

public sealed interface Trash permits JnaTrash, LinuxTrash {

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
