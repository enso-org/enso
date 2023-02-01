package org.enso.polyglot;

import org.graalvm.polyglot.HostAccess;

/** Utility class for creating HostAccess object. */
public class HostAccessFactory {

  // Copied from com.oracle.truffle.api.interop.NumberUtils
  // since the latter is inaccessible
  private static final long LONG_MAX_SAFE_DOUBLE = 9007199254740991L; // 2 ** 53 - 1

  public HostAccess allWithTypeMapping() {
    return HostAccess.newBuilder()
        .allowPublicAccess(true)
        .allowAllImplementations(true)
        .allowAllClassImplementations(true)
        .allowArrayAccess(true)
        .allowListAccess(true)
        .allowBufferAccess(true)
        .allowIterableAccess(true)
        .allowIteratorAccess(true)
        .allowMapAccess(true)
        .allowAccessInheritance(true)
        .targetTypeMapping(
            Long.class,
            Double.class,
            (v) -> v != null && !longInSafeDoubleRange(v, LONG_MAX_SAFE_DOUBLE),
            (v) -> Double.longBitsToDouble(v))
        .build();
  }

  private boolean longInSafeDoubleRange(Long v, Long max) {
    return v >= -max && v <= max;
  }
}
