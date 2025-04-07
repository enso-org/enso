package org.enso.change.directory;

public abstract class WorkingDirectory {
  WorkingDirectory() {}

  public abstract boolean changeWorkingDir(String path);

  public abstract String currentWorkingDir();

  public abstract boolean exists(String dir, String file);
}
