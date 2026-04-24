package org.enso.base.file_system;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.module.ModuleFinder;
import java.lang.module.ModuleReference;
import java.net.URISyntaxException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.nio.file.PathMatcher;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import org.graalvm.nativeimage.ImageInfo;

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

  /**
   * Returns canonical path to the enso executable location.
   *
   * @return the path - never {@code null}
   * @throws IOException when the path cannot be found
   */
  public static String getExecutableLocation() throws IOException, URISyntaxException {
    File exec;
    if (ImageInfo.inImageRuntimeCode()) {
      exec = getExecutableLocationInNI();
    } else {
      var runnerMod = findRunnerModule();
      if (runnerMod.isPresent()) {
        exec = getExecutableLocationFromModule(runnerMod.get());
      } else {
        throw new FileNotFoundException();
      }
    }
    return exec.getCanonicalPath();
  }

  private static File getExecutableLocationInNI() throws URISyntaxException {
    var codeSource = File_Utils.class.getProtectionDomain().getCodeSource();
    assert codeSource != null;
    var loc = codeSource.getLocation();
    return new File(loc.toURI());
  }

  private static Optional<ModuleReference> findRunnerModule() {
    var mpStr = System.getProperty("jdk.module.path");
    var modulePath = Arrays.stream(mpStr.split(File.pathSeparator)).map(Path::of);
    var modFinder = ModuleFinder.of(modulePath.toList().toArray(Path[]::new));
    return modFinder.find("org.enso.runner");
  }

  private static File getExecutableLocationFromModule(ModuleReference runnerMod)
      throws FileNotFoundException {
    var modLoc =
        runnerMod
            .location()
            .orElseThrow(() -> new AssertionError("Module org.enso.runner location not found"));
    var binDir = Path.of(modLoc).getParent().getParent().resolve("bin");
    if (!binDir.toFile().exists() || !binDir.toFile().isDirectory()) {
      throw new AssertionError(binDir + " is not a bin directory");
    }
    return resolveFromBinDir(binDir);
  }

  private static File resolveFromBinDir(Path binDir) throws FileNotFoundException {
    File exec;
    if (File.separatorChar == '\\') {
      var exe = binDir.resolve("enso.exe").toFile();
      if (exe.canExecute()) {
        exec = exe;
      } else {
        exec = binDir.resolve("enso.bat").toFile();
      }
    } else {
      exec = binDir.resolve("enso").toFile();
    }
    if (!(exec.exists() && exec.isFile() && exec.canExecute())) {
      throw new FileNotFoundException(exec + " is not a valid executable");
    }
    return exec;
  }
}
