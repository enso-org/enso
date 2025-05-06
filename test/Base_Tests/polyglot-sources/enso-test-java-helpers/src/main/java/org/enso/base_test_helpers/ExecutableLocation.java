package org.enso.base_test_helpers;

import java.io.File;
import java.io.IOException;
import java.lang.module.ModuleFinder;
import java.net.URISyntaxException;
import java.nio.file.Path;
import java.util.Arrays;
import org.graalvm.nativeimage.ImageInfo;

public final class ExecutableLocation {
  private ExecutableLocation() {}

  /** Returns canonical path to the enso executable location. */
  public static String getExecutableLocation() {
    File exec;
    if (ImageInfo.inImageRuntimeCode()) {
      exec = getExecutableLocationInNI();
    } else {
      exec = getExecutableLocationFromModule();
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

  private static File getExecutableLocationFromModule() {
    var mpStr = System.getProperty("jdk.module.path");
    var modulePath = Arrays.stream(mpStr.split(File.pathSeparator)).map(Path::of);
    var modFinder = ModuleFinder.of(modulePath.toList().toArray(Path[]::new));
    var runnerMod =
        modFinder
            .find("org.enso.runner")
            .orElseThrow(() -> new AssertionError("Module org.enso.runner not found"));
    var modLoc =
        runnerMod
            .location()
            .orElseThrow(() -> new AssertionError("Module org.enso.runner location not found"));
    var binDir = Path.of(modLoc).getParent().getParent().resolve("bin");
    if (!binDir.toFile().exists() || !binDir.toFile().isDirectory()) {
      throw new AssertionError(binDir + " is not a bin directory");
    }
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

  private static boolean isOnWindows() {
    String os = System.getProperty("os.name").toLowerCase();
    return os.contains("windows");
  }
}
