package org.enso.change.directory;

import org.enso.common.Platform;

public interface WorkingDirectory {
  static WorkingDirectory getForCurrentPlatform() {
    return switch (Platform.getOperatingSystem()) {
      case Platform.LINUX -> LinuxWorkingDirectory.INSTANCE;
      case Platform.MACOS -> throw new UnsupportedOperationException("unimplemented");
      case Platform.WINDOWS -> throw new UnsupportedOperationException("unimplemented");
    };
  }

  boolean changeWorkingDir(String path);

  String currentWorkingDir();

  boolean exists(String dir, String file);
}
