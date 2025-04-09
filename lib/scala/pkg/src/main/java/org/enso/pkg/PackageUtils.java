package org.enso.pkg;

import java.util.HashMap;
import java.util.Map;

/**
 * Contains data associated with packages.
 *
 * <p>This class has to be <em>initialized in build time</em> otherwise the modifications to {@code
 * AOT_READY} map by a native image feature will not be persist in heap.
 */
final class PackageUtils {
  /*
   * {@code EnsoLibraryFeature} registers libraries it processes into this map.
   * Value of this field gets compiled into <em>native image heap</em>.
   * When the system starts running in AOT mode, it has the registered values
   * available and can check if the libraries are "AOT ready".
   */
  private static final Map<Config, Boolean> AOT_READY = new HashMap<>();

  private PackageUtils() {}

  static void markAotReady(Config cfg) {
    AOT_READY.put(cfg, true);
  }

  static boolean isAotReady(Config cfg) {
    for (var k : AOT_READY.keySet()) {
      if (cfg.namespace().equals(k.namespace()) && cfg.name().equals(k.name())) {
        return true;
      }
    }
    return false;
  }
}
