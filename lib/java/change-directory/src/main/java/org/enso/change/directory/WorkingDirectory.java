package org.enso.change.directory;

public abstract class WorkingDirectory {
    WorkingDirectory() {}
  boolean changeWorkingDir(String path);

  String currentWorkingDir();

  boolean exists(String dir, String file);
}
