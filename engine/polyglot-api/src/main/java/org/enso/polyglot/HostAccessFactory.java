package org.enso.polyglot;

import org.graalvm.polyglot.HostAccess;

/** Utility class for creating HostAccess object. */
public class HostAccessFactory {
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
        .build();
  }
}
