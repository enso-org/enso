package org.enso.pkg;

import java.util.HashMap;
import java.util.Map;
import org.graalvm.nativeimage.ImageInfo;

/**
 * Contains data associated with packages.
 *
 * <p>This class has to be <em>initialized in build time</em> otherwise the modifications to {@code
 * AOT_READY} map by a native image feature will not be persist in heap.
 */
final class PackageUtils {
  static {
    if (ImageInfo.inImageRuntimeCode()) {
      throw new IllegalStateException("This class has to be initialized in built time!");
    }
  }

  private static final int INDEX_READY = 0;
  private static final int INDEX_WARNED = 1;

  /*
   * {@code EnsoLibraryFeature} registers libraries it processes into this map.
   * Value of this field gets compiled into <em>native image heap</em>.
   * When the system starts running in AOT mode, it has the registered values
   * available and can check if the libraries are "AOT ready".
   * <p>
   * @GuardedBy("PackageUtils.class")
   */
  private static final Map<Config, boolean[]> AOT_READY = new HashMap<>();

  private PackageUtils() {}

  private static synchronized boolean[] forConfig(Config cfg) {
    for (var entry : AOT_READY.entrySet()) {
      var k = entry.getKey();
      if (cfg.namespace().equals(k.namespace()) && cfg.name().equals(k.name())) {
        return entry.getValue();
      }
    }
    var res = new boolean[] {false, false};
    AOT_READY.put(cfg, res);
    return res;
  }

  static void markAotReady(Config cfg) {
    if (ImageInfo.inImageBuildtimeCode()) {
      var arr = forConfig(cfg);
      arr[INDEX_READY] = true;
    } else {
      throw new IllegalStateException("Can only markAotReady when building native image");
    }
  }

  static boolean isAotReady(Config cfg) {
    return forConfig(cfg)[INDEX_READY];
  }

  static boolean checkAotReady(Config cfg, Boolean newValue) {
    var arr = forConfig(cfg);
    var warned = arr[INDEX_WARNED];
    if (newValue != null) {
      // next time return new value
      arr[INDEX_WARNED] = newValue;
    }
    return warned;
  }
}
