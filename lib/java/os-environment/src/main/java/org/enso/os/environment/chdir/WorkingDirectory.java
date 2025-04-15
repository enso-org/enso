package org.enso.os.environment.chdir;

import org.enso.common.Platform;

/** Class responsible for changing the working directory via native code. */
public abstract sealed class WorkingDirectory
    permits PosixWorkingDirectory, WindowsWorkingDirectory {
  WorkingDirectory() {}

  private static WorkingDirectory INSTANCE;

  public static WorkingDirectory getInstance() {
    if (INSTANCE == null) {
      INSTANCE =
          switch (Platform.getOperatingSystem()) {
            case LINUX, MACOS -> PosixWorkingDirectory.INSTANCE;
            case WINDOWS -> WindowsWorkingDirectory.INSTANCE;
          };
    }
    return INSTANCE;
  }

  public abstract boolean changeWorkingDir(String path);

  public abstract String currentWorkingDir();

  public abstract boolean exists(String dir, String file);
}
