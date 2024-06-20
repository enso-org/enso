package org.enso.base.file_system;

import java.nio.file.FileSystems;
import java.nio.file.Path;
import java.util.function.Predicate;

public final class File_Utils {
  private File_Utils() {}

  public static Predicate<String> matchPath(String filter) {
    var fs = FileSystems.getDefault();
    var matcher = fs.getPathMatcher(filter);
    return (pathStr) -> {
      return matcher.matches(Path.of(pathStr));
    };
  }
}
