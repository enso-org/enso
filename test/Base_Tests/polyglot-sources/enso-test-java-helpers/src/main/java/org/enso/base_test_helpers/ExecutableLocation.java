package org.enso.base_test_helpers;

import java.io.File;
import java.io.IOException;
import java.lang.module.ModuleFinder;
import java.lang.module.ModuleReference;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Optional;
import org.graalvm.nativeimage.ImageInfo;

public final class ExecutableLocation {
  private ExecutableLocation() {}

  /** Returns canonical path to the enso executable location. */
  public static String getExecutableLocation() {
    File exec;
    if (ImageInfo.inImageRuntimeCode()) {
      exec = getExecutableLocationInNI();
    } else {
      var runnerMod = findRunnerModule();
      if (runnerMod.isPresent()) {
        exec = getExecutableLocationFromModule(runnerMod.get());
      } else {
        // Fallback to finding from project root
        exec = getExecutableLocationFromRepoRoot();
      }
    }
    try {
      return exec.getCanonicalPath();
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  private static File getExecutableLocationInNI() {
    var codeSource = ExecutableLocation.class.getProtectionDomain().getCodeSource();
    assert codeSource != null;
    var loc = codeSource.getLocation();
    try {
      return new File(loc.toURI());
    } catch (URISyntaxException e) {
      throw new AssertionError(e);
    }
  }

  private static Optional<ModuleReference> findRunnerModule() {
    var mpStr = System.getProperty("jdk.module.path");
    var modulePath = Arrays.stream(mpStr.split(File.pathSeparator)).map(Path::of);
    var modFinder = ModuleFinder.of(modulePath.toList().toArray(Path[]::new));
    return modFinder.find("org.enso.runner");
  }

  private static File getExecutableLocationFromModule(ModuleReference runnerMod) {
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

  private static File resolveFromBinDir(Path binDir) {
    File exec;
    if (isOnWindows()) {
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
      throw new AssertionError(exec + " is not a valid executable");
    }
    return exec;
  }

  private static File getExecutableLocationFromRepoRoot() {
    var repoRoot = findRepoRoot();
    var builtDistrDir = ensureExist(repoRoot.resolve("built-distribution"));
    var engineDir = ensureExist(findFileWithPrefix(builtDistrDir, "enso-engine-"));
    var ensoDir = ensureExist(findFileWithPrefix(engineDir, "enso-"));
    var binDir = ensureExist(ensoDir.resolve("bin"));
    return resolveFromBinDir(binDir);
  }

  private static Path ensureExist(Path dir) {
    if (!Files.exists(dir)) {
      throw new AssertionError(dir + " does not exist");
    } else {
      return dir;
    }
  }

  private static Path findFileWithPrefix(Path dir, String prefix) {
    try {
      return Files.list(dir)
          .filter(p -> p.getFileName().toString().startsWith(prefix))
          .findFirst()
          .orElseThrow(() -> new AssertionError("No file with prefix " + prefix + " in " + dir));
    } catch (IOException e) {
      throw new AssertionError(e);
    }
  }

  private static Path findRepoRoot() {
    var codeSource = ExecutableLocation.class.getProtectionDomain().getCodeSource();
    assert codeSource != null;
    var loc = codeSource.getLocation();
    try {
      var path = Path.of(loc.toURI());
      var dir = path.toFile().isDirectory() ? path : path.getParent();
      while (dir != null) {
        boolean hasGitDir;
        try {
          hasGitDir = Files.list(dir).anyMatch(p -> p.getFileName().toString().equals(".git"));
        } catch (IOException e) {
          throw new AssertionError(e);
        }
        if (hasGitDir) {
          return dir;
        } else {
          dir = dir.getParent();
        }
      }
      throw new AssertionError("Could not find repository root");
    } catch (URISyntaxException e) {
      throw new AssertionError(e);
    }
  }

  private static boolean isOnWindows() {
    String os = System.getProperty("os.name").toLowerCase();
    return os.contains("windows");
  }
}
