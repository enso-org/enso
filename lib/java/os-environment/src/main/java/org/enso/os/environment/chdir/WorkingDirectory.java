package org.enso.os.environment.chdir;

import org.enso.common.Platform;

/**
 * Class responsible for changing the working directory via native code.
 *
 * <p>Note that it is not possible to change the working directory in pure Java. It has to be done
 * via system calls.
 */
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

  /**
   * In order for the wdir change to be reflected by the JVM, it must be called before {@link
   * java.io.File} class is loaded.
   *
   * <p>In general, try to avoid accessing anything from {@code java.io} before this method is
   * called.
   *
   * @param path Absolute or relative path.
   * @return True if the working directory was changed, false if an error occurred.
   */
  public abstract boolean changeWorkingDir(String path);

  public abstract String currentWorkingDir();

  /**
   * Returns true if the given {@code file} exists in the given {@code dir}.
   *
   * @param dir Path to the directory. Absolute or relative. Not null. Does not have to end with
   *     path separator.
   * @param file Name of the file in the directory. Not null.
   * @return true if the file exists in the directory, false otherwise.
   */
  public abstract boolean exists(String dir, String file);

  /**
   * Attempts to find project root directory.
   *
   * @param path Can be absolute or relative. Not null.
   * @return null if project root was not found, a canonical path otherwise.
   */
  public final String findProjectRoot(String path) {
    assert path != null;
    var nativeApi = WorkingDirectory.getInstance();
    String curPath;
    if (isPathAbsolute(path)) {
      curPath = path;
    } else {
      curPath =
          nativeApi.currentWorkingDir()
              + Platform.separatorChar()
              + path.replace('/', Platform.separatorChar());
    }
    if (curPath.endsWith("" + Platform.separatorChar())) {
      curPath = curPath.substring(0, curPath.length() - 1);
    }
    while (curPath != null) {
      if (nativeApi.exists(curPath, "package.yaml") && nativeApi.exists(curPath, "src")) {
        return curPath;
      }
      curPath = parentFile(curPath);
    }
    return null;
  }

  public final String parentFile(String path) {
    var separatorChar = Platform.separatorChar();
    var lastSlash = path.lastIndexOf(separatorChar);
    if (lastSlash == -1) {
      return null;
    } else {
      return path.substring(0, lastSlash);
    }
  }

  private static boolean isPathAbsolute(String path) {
    return switch (Platform.getOperatingSystem()) {
      case LINUX, MACOS -> path.charAt(0) == Platform.separatorChar();
      case WINDOWS ->
          path.length() > 2 && path.charAt(1) == ':' && path.charAt(2) == Platform.separatorChar();
    };
  }
}
