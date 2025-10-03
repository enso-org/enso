package org.enso.table.util;

/**
 * A simple LRU cache implementation for internal optimisation.
 *
 * @param <K> the type of keys
 * @param <V> the type of values
 */
public final class LeastRecentlyUsedCache<K, V> extends java.util.LinkedHashMap<K, V> {
  private final int maxSize;

  /**
   * Constructs an LeastRecentlyUsedCache with the specified maximum size.
   *
   * @param maxSize the maximum size of the cache
   */
  public LeastRecentlyUsedCache(int maxSize) {
    super(maxSize, 0.75f, true);
    this.maxSize = maxSize;
  }

  @Override
  protected boolean removeEldestEntry(java.util.Map.Entry<K, V> eldest) {
    return size() > maxSize;
  }
}
