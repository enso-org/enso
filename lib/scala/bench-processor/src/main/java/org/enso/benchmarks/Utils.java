package org.enso.benchmarks;

import java.io.File;
import java.net.URISyntaxException;
import java.nio.file.Files;

/** Utility methods used by the benchmark classes from the generated code */
public class Utils {

  /**
   * Returns the path to the {@link org.enso.polyglot.RuntimeOptions#LANGUAGE_HOME_OVERRIDE language
   * home override directory}.
   *
   * <p>Note that the returned file may not exist.
   *
   * @return Non-null file pointing to the language home override directory.
   */
  public static File findLanguageHomeOverride() {
    File ensoDir = findRepoRootDir();
    // Note that ensoHomeOverride does not have to exist, only its parent directory
    return ensoDir.toPath().resolve("distribution").resolve("component").toFile();
  }

  /**
   * Locates the root of the Enso repository. Heuristic: we just keep going up the directory tree
   * until we are in a directory containing ".git" subdirectory. Note that we cannot use the "enso"
   * name, as users are free to name their cloned directories however they like.
   *
   * @return Non-null file pointing to the root directory of the Enso repository.
   */
  public static File findRepoRootDir() {
    File rootDir = null;
    try {
      rootDir = new File(Utils.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    } catch (URISyntaxException e) {
      throw new IllegalStateException("repository root directory not found: " + e.getMessage());
    }
    for (; rootDir != null; rootDir = rootDir.getParentFile()) {
      // Check if rootDir contains ".git" subdirectory
      if (Files.exists(rootDir.toPath().resolve(".git"))) {
        break;
      }
    }
    if (rootDir == null || !rootDir.exists() || !rootDir.isDirectory() || !rootDir.canRead()) {
      throw new IllegalStateException(
          "Unreachable: repository root directory does not exist or is not readable");
    }
    return rootDir;
  }

  public static BenchSpec findSpecByName(BenchGroup group, String specName) {
    for (BenchSpec spec : group.specs()) {
      if (spec.name().equals(specName)) {
        return spec;
      }
    }
    return null;
  }
}
