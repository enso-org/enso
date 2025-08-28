package org.enso.interpreter.caches;

import java.util.ArrayList;
import java.util.List;

/** Utility class to keep track of cache-related statistics. */
public final class CacheStatistics {
  private CacheStatistics() {}

  private final List<CacheEvent> cacheEvents = new ArrayList<>();

  public static CacheStatistics create() {
    return new CacheStatistics();
  }

  public void addEvent(CacheEvent event) {
    synchronized (cacheEvents) {
      cacheEvents.add(event);
    }
  }

  public List<CacheEvent> getCacheEvents() {
    return cacheEvents;
  }
}
