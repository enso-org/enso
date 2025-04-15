package org.enso.os.environment;

import org.enso.os.environment.directories.Directories;
import org.enso.os.environment.trash.TrashBin;

public final class DesktopEnvironment {
  private static final Directories DIRECTORIES = Directories.getCurrent();
  private static final TrashBin TRASH_BIN = TrashBin.getCurrent();

  private DesktopEnvironment() {}

  public static Directories getDirectories() {
    return DIRECTORIES;
  }

  public static TrashBin getTrashBin() {
    return TRASH_BIN;
  }
}
