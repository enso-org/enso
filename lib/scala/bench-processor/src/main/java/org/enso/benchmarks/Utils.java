package org.enso.benchmarks;

import java.io.File;
import java.net.URISyntaxException;

/**
 * Used by the benchmark classes from the generated code
 */
public class Utils {
  public static String findLanguageHomeOverride() {
    File ensoDir;
    try {
      ensoDir =
          new File(
              Utils.class.getProtectionDomain().getCodeSource().getLocation().toURI());
    } catch (URISyntaxException e) {
      throw new IllegalStateException("Unrecheable: ensoDir not found", e);
    }
    for (; ensoDir != null; ensoDir = ensoDir.getParentFile()) {
      if (ensoDir.getName().equals("enso")) {
        break;
      }
    }
    assert ensoDir != null;
    assert ensoDir.exists();
    assert ensoDir.isDirectory();
    assert ensoDir.canRead();

    // Note that ensoHomeOverride does not have to exist, only its parent directory
    File ensoHomeOverride = ensoDir.toPath().resolve("distribution").resolve("component").toFile();
    return ensoHomeOverride.getAbsolutePath();
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
