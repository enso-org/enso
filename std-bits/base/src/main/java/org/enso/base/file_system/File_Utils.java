package org.enso.base.file_system;

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.List;

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

  public static void delete(Path path, boolean recursive) throws IOException {
    if (recursive && Files.isDirectory(path, LinkOption.NOFOLLOW_LINKS)) {
      deleteRecursively(path);
    } else {
      Files.delete(path);
    }
  }

  public static String getPosixPermissions(Path path) throws IOException {
    return PosixFilePermissions.toString(Files.getPosixFilePermissions(path));
  }

  public static List<Path> listImmediateChildren(Path dir) throws IOException {
    try (var stream = Files.list(dir)) {
      return stream.toList();
    }
  }

  private static void deleteRecursively(Path file) throws IOException {
    if (Files.isDirectory(file, LinkOption.NOFOLLOW_LINKS)) {
      try (var entries = Files.newDirectoryStream(file)) {
        for (var entry : entries) {
          deleteRecursively(entry);
        }
      }
    }
    Files.delete(file);
  }
}
