package org.enso.interpreter.caches;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Utility class to keep track of cache-related counters.
 */
public final class CacheCounters {
  private CacheCounters() {}

  private final List<String> filesWithDigestComputation = new ArrayList<>();

  public static CacheCounters create() {
    return new CacheCounters();
  }

  /**
   * Called when a source file is read and digest is computed from its content.
   */
  void digestForSourceFile(String filePath) {
    filesWithDigestComputation.add(filePath);
  }

  public List<String> getFilesWithDigestComputation() {
    return Collections.unmodifiableList(filesWithDigestComputation);
  }
}
