package org.enso.benchmarks;

import java.io.File;
import java.net.URISyntaxException;

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
   * Returns the root directory of the Enso repository.
   *
   * @return Non-null file pointing to the root directory of the Enso repository.
   */
  public static File findRepoRootDir() {
    File ensoDir;
    try {
      ensoDir = new File(Utils.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    } catch (URISyntaxException e) {
      throw new IllegalStateException("Unrecheable: ensoDir not found", e);
    }
    for (; ensoDir != null; ensoDir = ensoDir.getParentFile()) {
      if (ensoDir.getName().equals("enso")) {
        break;
      }
    }
    if (ensoDir == null || !ensoDir.exists() || !ensoDir.isDirectory() || !ensoDir.canRead()) {
      throw new IllegalStateException("Unrecheable: ensoDir does not exist or is not readable");
    }
    return ensoDir;
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
