package org.enso.change.directory;

public interface WorkingDirectory {
  boolean changeWorkingDir(String path);

  String currentWorkingDir();

  boolean exists(String dir, String file);
}
