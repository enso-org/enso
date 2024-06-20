package org.enso.base.file_system;

import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.nio.file.PathMatcher;

public final class File_Utils {
  private File_Utils() {}

  public static Path toPath(String path) {
    return Path.of(path);
  }

  public static PathMatcher matchPath(String filter) {
    var fs = FileSystems.getDefault();
    var matcher = fs.getPathMatcher(filter);
    return matcher;
  }

  public static boolean matches(PathMatcher matcher, String pathStr) {
    return matcher.matches(Path.of(pathStr));
  }
}
