package org.enso.change.directory;

import org.enso.common.Platform;

public final class WorkingDirectories {
  private WorkingDirectories() {}

  private static final WorkingDirectory CURRENT =
      switch (Platform.getOperatingSystem()) {
        case LINUX, MACOS -> PosixWorkingDirectory.INSTANCE;
        case WINDOWS -> WindowsWorkingDirectory.INSTANCE;
      };

  public static WorkingDirectory getCurrent() {
    return CURRENT;
  }
}
