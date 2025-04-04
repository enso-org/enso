package org.enso.change.directory;

import java.io.File;

public abstract class WorkingDirectory {
  public abstract boolean changeWorkingDir(String path);

  public abstract String currentWorkingDir();

  public abstract boolean isDirectory(String path);

  /**
   * Checks existence of a file in dir.
   *
   * @param dir If null, {@code file} is treated as an absolute path and its existance is checked
   *     via native code.
   * @param file Not null. Either absolute path to a file, or a file name in dir.
   */
  public boolean exists(String dir, String file) {
    String full;
    if (dir == null) {
      full = file;
    } else if (dir.endsWith(File.separator)) {
      full = dir + file;
    } else {
      full = dir + File.separator + file;
    }
    return existsImpl(full);
  }

  abstract boolean existsImpl(String fullPath);
}
