package org.enso.change.directory;

import org.enso.common.Platform;

public interface WorkingDirectory {
  static WorkingDirectory getForCurrentPlatform() {
    return switch (Platform.getOperatingSystem()) {
      case Platform.LINUX, Platform.MACOS -> PosixWorkingDirectory.INSTANCE;
      case Platform.WINDOWS -> WindowsWorkingDirectory.INSTANCE;
    };
  }

  boolean changeWorkingDir(String path);

  String currentWorkingDir();

  boolean exists(String dir, String file);
}
